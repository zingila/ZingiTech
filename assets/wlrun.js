/* wlrun.js · interpréteur W-Langage du bac à sable ZingiTech
   Sous-ensemble : variables typées, tableaux, SI / SINON SI, SELON, POUR, POUR TOUT,
   TANTQUE, BOUCLE, procédures internes, fonctions usuelles de chaîne et de calcul, Trace.
   Comme le W-Langage, le langage est insensible à la casse et aux accents des mots-clés.
   API : window.wlRun(source) → { out: [lignes], err: message | null, steps: nombre } */
(function(){
'use strict';
var MAX_STEPS=200000,MAX_OUT=500,MAX_DEPTH=200;

function norm(s){return String(s).normalize('NFD').replace(/[̀-ͯ]/g,'').toLowerCase();}
function Err(msg,line){this.message=msg;this.line=line;}
function fail(msg,line){throw new Err(msg,line);}

/* ---------- analyse lexicale ---------- */
var ID0=/[A-Za-zÀ-ÖØ-öø-ɏ_]/,ID1=/[A-Za-zÀ-ÖØ-öø-ɏ0-9_]/;
var OPS=['++','--','+=','-=','*=','/=','<=','>=','<>','~=','~~','..','=','<','>','+','-','*','/','%','(',')','[',']',',',':','.',';'];
function lex(src){
  var t=[],i=0,n,line=1,c,j;
  src=String(src).replace(/\r\n?/g,'\n');n=src.length;
  while(i<n){
    c=src[i];
    if(c==='\n'){t.push({k:'nl',line:line});line++;i++;continue;}
    if(c===' '||c==='\t'||c===' '){i++;continue;}
    if(c==='/'&&src[i+1]==='/'){while(i<n&&src[i]!=='\n')i++;continue;}
    if(c==='/'&&src[i+1]==='*'){j=src.indexOf('*/',i+2);if(j<0)j=n;for(;i<j;i++)if(src[i]==='\n')line++;i=j+2;continue;}
    if(c==='"'){j=i+1;while(j<n&&src[j]!=='"'&&src[j]!=='\n')j++;if(src[j]!=='"')fail('Chaîne non terminée : il manque un guillemet fermant.',line);t.push({k:'str',v:src.slice(i+1,j),line:line});i=j+1;continue;}
    if(/[0-9]/.test(c)){j=i;while(j<n&&/[0-9]/.test(src[j]))j++;if(src[j]==='.'&&/[0-9]/.test(src[j+1]||'')){j++;while(j<n&&/[0-9]/.test(src[j]))j++;}t.push({k:'num',v:parseFloat(src.slice(i,j)),line:line});i=j;continue;}
    if(ID0.test(c)){j=i;while(j<n&&ID1.test(src[j]))j++;var w=src.slice(i,j);t.push({k:'id',v:w,n:norm(w),line:line});i=j;continue;}
    var op=null;for(var q=0;q<OPS.length;q++){if(src.substr(i,OPS[q].length)===OPS[q]){op=OPS[q];break;}}
    if(!op)fail('Caractère inattendu : « '+c+' ».',line);
    t.push({k:'op',v:op,line:line});i+=op.length;
  }
  t.push({k:'eof',line:line});
  return t;
}

/* ---------- analyse syntaxique ---------- */
var TYPES={entier:'entier',entiers:'entier',reel:'reel',reels:'reel',monetaire:'reel',monetaires:'reel',numerique:'reel',numeriques:'reel',chaine:'chaine',chaines:'chaine',caractere:'chaine',caracteres:'chaine',booleen:'booleen',booleens:'booleen',variant:'variant',variants:'variant',date:'chaine',dates:'chaine',heure:'chaine',heures:'chaine',dateheure:'chaine'};

function parse(tokens,procs){
  var p=0;
  function peek(o){return tokens[Math.min(p+(o||0),tokens.length-1)];}
  function next(){return tokens[p++];}
  function isKw(t,w){return t.k==='id'&&t.n===w;}
  function isOp(t,o){return t.k==='op'&&t.v===o;}
  function kw(w){if(isKw(peek(),w)){p++;return true;}return false;}
  function op(o){if(isOp(peek(),o)){p++;return true;}return false;}
  function show(t){return t.k==='nl'?'fin de ligne':t.k==='eof'?'fin du programme':t.k==='str'?'"'+t.v+'"':String(t.v);}
  function expectKw(w,label){if(!kw(w))fail('« '+(label||w.toUpperCase())+' » attendu, trouvé « '+show(peek())+' ».',peek().line);}
  function expectOp(o){if(!op(o))fail('« '+o+' » attendu, trouvé « '+show(peek())+' ».',peek().line);}
  function ident(){var t=peek();if(t.k!=='id')fail('Nom attendu, trouvé « '+show(t)+' ».',t.line);p++;return t;}
  function atEnd(){var t=peek();return t.k==='nl'||t.k==='eof'||isOp(t,';');}
  function skipSep(){while(peek().k==='nl'||isOp(peek(),';'))p++;}
  function endStmt(){var t=peek();if(t.k==='nl'||isOp(t,';')){p++;return;}if(t.k==='eof')return;fail('Fin d\'instruction attendue avant « '+show(t)+' ».',t.line);}

  function block(terms,open){
    var body=[];
    for(;;){
      skipSep();var t=peek();
      if(t.k==='eof'){if(open)fail('FIN manquant pour le bloc '+open.w+' ouvert ligne '+open.line+'.',t.line);return body;}
      if(t.k==='id'&&terms.indexOf(t.n)>=0)return body;
      body.push(stmt());endStmt();
    }
  }

  function stmt(){
    var t=peek(),L=t.line;
    if(t.k==='id'){
      switch(t.n){
        case 'si':p++;return ifRest(L);
        case 'pour':p++;return forStmt(L);
        case 'tantque':{p++;var c=expr();var b=block(['fin'],{w:'TANTQUE',line:L});expectKw('fin');return {type:'while',cond:c,body:b,line:L};}
        case 'boucle':{p++;var cnt=null;if(op('(')){cnt=expr();expectOp(')');}var bb=block(['fin'],{w:'BOUCLE',line:L});expectKw('fin');return {type:'loop',count:cnt,body:bb,line:L};}
        case 'selon':p++;return switchStmt(L);
        case 'procedure':p++;return procDecl(L);
        case 'renvoyer':case 'retour':p++;return {type:'ret',e:(atEnd()||isKw(peek(),'sinon'))?null:expr(),line:L};
        case 'sortir':p++;return {type:'brk',line:L};
        case 'continuer':p++;return {type:'cont',line:L};
        case 'fin':case 'sinon':case 'alors':fail('« '+t.v+' » inattendu ici : aucun bloc ouvert ne lui correspond.',L);
      }
      if(isDecl())return decl(L);
    }
    return simple(L);
  }

  function isDecl(){
    var q=p+1;
    while(isOp(tokens[q],',')&&tokens[q+1]&&tokens[q+1].k==='id')q+=2;
    var t=tokens[q];return t&&t.k==='id'&&(t.n==='est'||t.n==='sont');
  }
  function typeWords(stopAtEq){
    var words=[];
    while(!atEnd()&&!(stopAtEq&&isOp(peek(),'='))&&!isOp(peek(),',')&&!isOp(peek(),')')){var t=next();words.push(t.k==='id'?t.n:t.k==='num'?String(t.v):t.v);}
    var ty={t:'variant',elem:'variant',size:0};
    if(words.indexOf('tableau')>=0){ty.t='tab';for(var i=0;i<words.length;i++){if(TYPES[words[i]])ty.elem=TYPES[words[i]];if(/^[0-9]+$/.test(words[i]))ty.size=parseInt(words[i],10);}}
    else{for(var k=0;k<words.length;k++){if(TYPES[words[k]]){ty.t=TYPES[words[k]];break;}}}
    return ty;
  }
  function decl(L){
    var names=[ident().v];
    while(op(','))names.push(ident().v);
    next();// est / sont
    while(isKw(peek(),'un')||isKw(peek(),'une')||isKw(peek(),'des'))p++;
    var ty=typeWords(true),init=null;
    if(op('='))init=expr();
    return {type:'decl',names:names,ty:ty,init:init,line:L};
  }

  function ifRest(L){
    var cond=expr();kw('alors');
    if(!atEnd()){
      var th=[stmt()],el=null;
      if(kw('sinon'))el=[stmt()];
      return {type:'if',cond:cond,then:th,els:el,line:L};
    }
    var thenB=block(['sinon','fin'],{w:'SI',line:L}),elseB=null;
    if(kw('sinon')){
      if(isKw(peek(),'si')){var l2=next().line;return {type:'if',cond:cond,then:thenB,els:[ifRest(l2)],line:L};}
      elseB=block(['fin'],{w:'SINON',line:L});
    }
    expectKw('fin');
    return {type:'if',cond:cond,then:thenB,els:elseB,line:L};
  }

  function forStmt(L){
    if(kw('tout')){
      kw('element');
      if(kw('chaine')){
        var sv=ident().v;expectKw('de');var se=expr();expectKw('separee','SÉPARÉE');expectKw('par');var sep=expr();
        var sb=block(['fin'],{w:'POUR TOUT',line:L});expectKw('fin');
        return {type:'forsplit',v:sv,src:se,sep:sep,body:sb,line:L};
      }
      var v=ident().v,iv=null;if(op(','))iv=ident().v;
      if(!kw('de'))expectKw('dans','DE');
      var e=expr();
      var body=block(['fin'],{w:'POUR TOUT',line:L});expectKw('fin');
      return {type:'foreach',v:v,iv:iv,e:e,body:body,line:L};
    }
    var name=ident().v;expectOp('=');var from=expr();
    if(!kw('_a_')&&!kw('a'))fail('« _À_ » attendu dans la boucle POUR, trouvé « '+show(peek())+' ».',peek().line);
    var to=expr(),stp=null;if(kw('pas'))stp=expr();
    var b=block(['fin'],{w:'POUR',line:L});expectKw('fin');
    return {type:'for',v:name,from:from,to:to,step:stp,body:b,line:L};
  }

  function switchStmt(L){
    var subj=expr(),cases=[],def=null;
    for(;;){
      skipSep();var t=peek();
      if(t.k==='eof')fail('FIN manquant pour le bloc SELON ouvert ligne '+L+'.',t.line);
      if(kw('fin'))break;
      if(kw('autre')){expectKw('cas');op(':');def=block(['cas','autre','fin'],{w:'SELON',line:L});continue;}
      if(!kw('cas'))fail('« CAS » attendu dans le bloc SELON, trouvé « '+show(t)+' ».',t.line);
      var conds=[];
      do{
        var pk=peek();
        if(pk.k==='op'&&['<','>','<=','>=','<>'].indexOf(pk.v)>=0){p++;conds.push({op:pk.v,e:expr()});}
        else{var a=expr();if(kw('_a_')||kw('a'))conds.push({lo:a,hi:expr()});else conds.push({op:'=',e:a});}
      }while(op(','));
      op(':');
      cases.push({conds:conds,body:block(['cas','autre','fin'],{w:'SELON',line:L})});
    }
    return {type:'switch',subj:subj,cases:cases,def:def,line:L};
  }

  function procDecl(L){
    var internal=kw('interne'),name=ident(),params=[];
    if(op('(')){
      while(!op(')')){
        kw('local');var pn=ident().v,pt=null,def=null;
        if(kw('est')){while(isKw(peek(),'un')||isKw(peek(),'une'))p++;pt=typeWords(true);}
        if(op('='))def=expr();
        params.push({name:pn,ty:pt,def:def});
        if(!isOp(peek(),')'))expectOp(',');
      }
    }
    if(op(':'))while(!atEnd())p++;
    var body;
    if(internal){body=block(['fin'],{w:'PROCÉDURE INTERNE',line:L});expectKw('fin');}
    else body=block(['procedure'],null);
    procs[name.n]={name:name.v,params:params,body:body,line:L};
    return {type:'nop',line:L};
  }

  function simple(L){
    var save=p;
    if(peek().k==='id'){
      var lv=lvalue(),t=peek();
      if(lv&&t.k==='op'){
        if(['=','+=','-=','*=','/='].indexOf(t.v)>=0){p++;return {type:'assign',target:lv,op:t.v,e:expr(),line:L};}
        if(t.v==='++'||t.v==='--'){p++;return {type:'assign',target:lv,op:t.v==='++'?'+=':'-=',e:{type:'num',v:1},line:L};}
      }
      p=save;
    }
    var e=expr();
    return {type:'expr',e:e,line:L};
  }
  function lvalue(){
    var t=next(),node={type:'var',name:t.v,n:t.n,line:t.line};
    while(isOp(peek(),'[')){p++;var i=expr();expectOp(']');node={type:'idx',o:node,i:i,line:t.line};}
    return node;
  }

  /* expressions */
  function expr(){return orE();}
  function orE(){var l=andE();while(isKw(peek(),'ou')||isKw(peek(),'_ou_')){var t=next();l={type:'bin',op:'ou',l:l,r:andE(),line:t.line};}return l;}
  function andE(){var l=notE();while(isKw(peek(),'et')||isKw(peek(),'_et_')){var t=next();l={type:'bin',op:'et',l:l,r:notE(),line:t.line};}return l;}
  function notE(){if(isKw(peek(),'pas')){var t=next();return {type:'un',op:'pas',e:notE(),line:t.line};}return cmpE();}
  function cmpE(){var l=addE(),t=peek();if(t.k==='op'&&['=','<>','<','>','<=','>=','~=','~~'].indexOf(t.v)>=0){p++;l={type:'bin',op:t.v,l:l,r:addE(),line:t.line};}return l;}
  function addE(){var l=mulE();for(;;){var t=peek();if(isOp(t,'+')||isOp(t,'-')){p++;l={type:'bin',op:t.v,l:l,r:mulE(),line:t.line};}else return l;}}
  function mulE(){var l=unE();for(;;){var t=peek();if(isOp(t,'*')||isOp(t,'/')||isOp(t,'%')){p++;l={type:'bin',op:t.v,l:l,r:unE(),line:t.line};}else if(isKw(t,'modulo')){p++;l={type:'bin',op:'%',l:l,r:unE(),line:t.line};}else return l;}}
  function unE(){var t=peek();if(isOp(t,'-')){p++;return {type:'un',op:'-',e:unE(),line:t.line};}if(isOp(t,'+')){p++;return unE();}return post();}
  function post(){
    var e=prim();
    for(;;){
      var t=peek();
      if(isOp(t,'[')){p++;var i=expr();expectOp(']');e={type:'idx',o:e,i:i,line:t.line};}
      else if(isOp(t,'.')||isOp(t,'..')){p++;var pr=ident();e={type:'prop',o:e,p:pr.n,name:pr.v,line:t.line};}
      else return e;
    }
  }
  function prim(){
    var t=next();
    if(t.k==='num')return {type:'num',v:t.v};
    if(t.k==='str')return {type:'str',v:t.v};
    if(isOp(t,'(')){var e=expr();expectOp(')');return e;}
    if(isOp(t,'[')){var items=[];if(!isOp(peek(),']')){do{items.push(expr());}while(op(','));}expectOp(']');return {type:'arr',items:items,line:t.line};}
    if(t.k==='id'){
      if(t.n==='vrai')return {type:'lit',v:true};
      if(t.n==='faux')return {type:'lit',v:false};
      if(t.n==='null')return {type:'lit',v:null};
      if(isOp(peek(),'(')){p++;var args=[];if(!isOp(peek(),')')){do{args.push(expr());}while(op(','));}expectOp(')');return {type:'call',name:t.v,n:t.n,args:args,line:t.line};}
      return {type:'var',name:t.v,n:t.n,line:t.line};
    }
    fail('Expression attendue, trouvé « '+show(t)+' ».',t.line);
  }

  var prog=block([],null);
  return prog;
}

/* ---------- valeurs ---------- */
function isTab(v){return v!==null&&typeof v==='object'&&v.wl==='tab';}
function fmtNum(x){if(!isFinite(x))return String(x);if(Number.isInteger(x))return String(x);return String(parseFloat(x.toPrecision(15)));}
function toStr(v){if(v===null||v===undefined)return '';if(typeof v==='string')return v;if(typeof v==='number')return fmtNum(v);if(typeof v==='boolean')return v?'1':'0';if(isTab(v))return v.items.map(toStr).join(', ');return String(v);}
function numLike(s){return typeof s==='string'&&/^\s*[-+]?[0-9]+([.,][0-9]+)?\s*$/.test(s);}
function toNum(v,line){if(typeof v==='number')return v;if(typeof v==='boolean')return v?1:0;if(v===null||v===undefined||v==='')return 0;if(typeof v==='string'){var f=parseFloat(v.replace(',','.'));return isNaN(f)?0:f;}fail('Un tableau ne peut pas être utilisé dans un calcul.',line);}
function truthy(v){if(typeof v==='boolean')return v;if(typeof v==='number')return v!==0;if(typeof v==='string')return v!==''&&v!=='0';return !!v;}
function defVal(t){return t==='entier'||t==='reel'?0:t==='chaine'?'':t==='booleen'?false:null;}
function coerce(t,v,line){
  if(isTab(v)&&t!=='variant'&&t!=='tab')fail('Un tableau ne peut pas être affecté à une variable de type '+t+'.',line);
  switch(t){
    case 'entier':return Math.trunc(toNum(v,line));
    case 'reel':return toNum(v,line);
    case 'chaine':return toStr(v);
    case 'booleen':return truthy(v);
    default:return v;
  }
}
function newTab(elem,items){return {wl:'tab',elem:elem||'variant',items:items||[]};}
function copyTab(v,elem,line){var e=elem||v.elem;return newTab(e,v.items.map(function(x){return isTab(x)?copyTab(x,null,line):coerce(e,x,line);}));}

/* ---------- formats ---------- */
function numFormat(x,f,line){
  if(f===undefined)return toStr(x);
  var m=/^([0+ -]*)([0-9]*)(?:\.([0-9]+))?([dfFeExX])?$/.exec(String(f));
  if(!m)fail('Format « '+f+' » non pris en charge par NumériqueVersChaîne.',line);
  var flags=m[1],w=parseInt(m[2]||'0',10),pr=m[3],ty=m[4]||(pr!==undefined?'f':'d'),n=toNum(x,line),s;
  if(ty==='d')s=String(Math.round(Math.abs(n)));
  else if(ty==='f'||ty==='F')s=Math.abs(n).toFixed(pr!==undefined?parseInt(pr,10):6);
  else if(ty==='e'||ty==='E')s=Math.abs(n).toExponential(pr!==undefined?parseInt(pr,10):6);
  else s=Math.round(Math.abs(n)).toString(16);
  if(ty==='X'||ty==='E')s=s.toUpperCase();
  var sign=n<0&&parseFloat(s)!==0?'-':(flags.indexOf('+')>=0?'+':'');
  if(flags.indexOf('0')>=0&&flags.indexOf('-')<0){while(sign.length+s.length<w)s='0'+s;s=sign+s;}
  else{s=sign+s;if(flags.indexOf('-')>=0){while(s.length<w)s+=' ';}else{while(s.length<w)s=' '+s;}}
  return s;
}
function round(x,n){n=n||0;var a=Math.abs(x),r=Number(Math.round(Number(a+'e'+n))+'e'+(-n));return x<0?-r:r;}
function pad2(n){return (n<10?'0':'')+n;}

/* ---------- exécution ---------- */
function Brk(){}function Cont(){}function Ret(v){this.v=v;}

window.wlRun=function(source){
  var out=[],steps=0,procs={},depth=0;
  function tick(line){if(++steps>MAX_STEPS)fail('Exécution interrompue après '+MAX_STEPS.toLocaleString('fr-FR')+' instructions. Boucle infinie ?',line);}
  function emit(s,line){String(s).split(/\r?\n|\r/).forEach(function(l){if(out.length>=MAX_OUT)fail('Trace limitée à '+MAX_OUT+' lignes.',line);out.push(l);});}

  function Scope(parent){this.vars={};this.parent=parent;}
  Scope.prototype.find=function(n){var s=this;while(s){if(Object.prototype.hasOwnProperty.call(s.vars,n))return s.vars[n];s=s.parent;}return null;};
  var global=new Scope(null);

  var CONSTS={rc:'\n',tab:'\t',ttcroissant:1,ttdecroissant:2,pi:Math.PI};

  function argc(a,min,max,name,line){if(a.length<min||a.length>max)fail(name+' attend '+(min===max?min:min+' à '+max)+' paramètre'+(max>1?'s':'')+', '+a.length+' reçu'+(a.length>1?'s':'')+'.',line);}
  function needTab(v,name,line){if(!isTab(v))fail(name+' attend un tableau en premier paramètre.',line);return v;}
  function S(v){return toStr(v);}
  var B={
    trace:function(a,l){emit(a.map(S).join(' '),l);return null;},
    info:function(a,l){emit(a.map(S).join(' '),l);return null;},
    erreur:function(a,l){fail(a.map(S).join(' '),l);},
    tableauoccurrence:function(a,l){argc(a,1,1,'TableauOccurrence',l);return needTab(a[0],'TableauOccurrence',l).items.length;},
    tableauajoute:function(a,l){argc(a,2,2,'TableauAjoute',l);var t=needTab(a[0],'TableauAjoute',l);t.items.push(isTab(a[1])?copyTab(a[1],null,l):coerce(t.elem,a[1],l));return t.items.length;},
    ajoute:function(a,l){return B.tableauajoute(a,l);},
    tableauinsere:function(a,l){argc(a,3,3,'TableauInsère',l);var t=needTab(a[0],'TableauInsère',l),i=Math.trunc(toNum(a[1],l));if(i<1||i>t.items.length+1)fail('Indice '+i+' hors du tableau ('+t.items.length+' éléments).',l);t.items.splice(i-1,0,coerce(t.elem,a[2],l));return null;},
    tableausupprime:function(a,l){argc(a,2,2,'TableauSupprime',l);var t=needTab(a[0],'TableauSupprime',l),i=Math.trunc(toNum(a[1],l));if(i<1||i>t.items.length)fail('Indice '+i+' hors du tableau ('+t.items.length+' éléments).',l);t.items.splice(i-1,1);return null;},
    tableausupprimetout:function(a,l){argc(a,1,1,'TableauSupprimeTout',l);needTab(a[0],'TableauSupprimeTout',l).items.length=0;return null;},
    tableautrie:function(a,l){argc(a,1,2,'TableauTrie',l);var t=needTab(a[0],'TableauTrie',l),desc=a.length>1&&toNum(a[1],l)===2;t.items.sort(function(x,y){var r=(typeof x==='number'&&typeof y==='number')?x-y:S(x).localeCompare(S(y),'fr');return desc?-r:r;});return null;},
    tableaucherche:function(a,l){argc(a,2,3,'TableauCherche',l);var t=needTab(a[0],'TableauCherche',l),v=a[a.length-1];for(var i=0;i<t.items.length;i++)if(eq(t.items[i],v))return i+1;return -1;},
    taille:function(a,l){argc(a,1,1,'Taille',l);return isTab(a[0])?a[0].items.length:S(a[0]).length;},
    longueur:function(a,l){return B.taille(a,l);},
    numeriqueverschaine:function(a,l){argc(a,1,2,'NumériqueVersChaîne',l);return numFormat(a[0],a[1],l);},
    chaineconstruit:function(a,l){if(!a.length)fail('ChaîneConstruit attend au moins un paramètre.',l);return S(a[0]).replace(/%([1-9])/g,function(m,k){return k<a.length?S(a[k]):m;});},
    complete:function(a,l){argc(a,2,3,'Complète',l);var s=S(a[0]),n=Math.trunc(toNum(a[1],l)),c=a.length>2?S(a[2]).charAt(0)||' ':' ';if(s.length>=n)return s.slice(0,n);while(s.length<n)s+=c;return s;},
    majuscule:function(a,l){argc(a,1,1,'Majuscule',l);return S(a[0]).toUpperCase();},
    minuscule:function(a,l){argc(a,1,1,'Minuscule',l);return S(a[0]).toLowerCase();},
    sansespace:function(a,l){argc(a,1,1,'SansEspace',l);return S(a[0]).trim();},
    gauche:function(a,l){argc(a,2,2,'Gauche',l);return S(a[0]).slice(0,Math.max(0,toNum(a[1],l)));},
    droite:function(a,l){argc(a,2,2,'Droite',l);var s=S(a[0]),n=Math.max(0,toNum(a[1],l));return n?s.slice(-n):'';},
    milieu:function(a,l){argc(a,2,3,'Milieu',l);var s=S(a[0]),d=Math.max(1,Math.trunc(toNum(a[1],l)));return a.length>2?s.substr(d-1,Math.max(0,toNum(a[2],l))):s.slice(d-1);},
    position:function(a,l){argc(a,2,3,'Position',l);var d=a.length>2?Math.max(1,Math.trunc(toNum(a[2],l))):1;return S(a[0]).indexOf(S(a[1]),d-1)+1;},
    remplace:function(a,l){argc(a,3,3,'Remplace',l);var f=S(a[1]);return f?S(a[0]).split(f).join(S(a[2])):S(a[0]);},
    repete:function(a,l){argc(a,2,2,'Répète',l);var n=Math.trunc(toNum(a[1],l));if(n>10000)fail('Répète : nombre de répétitions trop grand.',l);return n>0?new Array(n+1).join(S(a[0])):'';},
    extraitchaine:function(a,l){argc(a,2,3,'ExtraitChaîne',l);var parts=S(a[0]).split(a.length>2?S(a[2]):'\t'),i=Math.trunc(toNum(a[1],l));return i>=1&&i<=parts.length?parts[i-1]:'';},
    val:function(a,l){argc(a,1,1,'Val',l);return toNum(S(a[0]),l);},
    caract:function(a,l){argc(a,1,1,'Caract',l);return String.fromCharCode(toNum(a[0],l));},
    asc:function(a,l){argc(a,1,1,'Asc',l);var s=S(a[0]);return s?s.charCodeAt(0):0;},
    arrondi:function(a,l){argc(a,1,2,'Arrondi',l);return round(toNum(a[0],l),a.length>1?Math.trunc(toNum(a[1],l)):0);},
    tronque:function(a,l){argc(a,1,1,'Tronque',l);return Math.trunc(toNum(a[0],l));},
    partieentiere:function(a,l){argc(a,1,1,'PartieEntière',l);return Math.floor(toNum(a[0],l));},
    abs:function(a,l){argc(a,1,1,'Abs',l);return Math.abs(toNum(a[0],l));},
    racine:function(a,l){argc(a,1,1,'Racine',l);var x=toNum(a[0],l);if(x<0)fail('Racine d\'un nombre négatif.',l);return Math.sqrt(x);},
    puissance:function(a,l){argc(a,2,2,'Puissance',l);return Math.pow(toNum(a[0],l),toNum(a[1],l));},
    max:function(a,l){if(!a.length)fail('Max attend au moins un paramètre.',l);return Math.max.apply(null,a.map(function(x){return toNum(x,l);}));},
    min:function(a,l){if(!a.length)fail('Min attend au moins un paramètre.',l);return Math.min.apply(null,a.map(function(x){return toNum(x,l);}));},
    hasard:function(a,l){argc(a,1,2,'Hasard',l);var lo=a.length>1?toNum(a[0],l):1,hi=toNum(a[a.length-1],l);return lo+Math.floor(Math.random()*(hi-lo+1));},
    datesys:function(a,l){argc(a,0,0,'DateSys',l);var d=new Date();return d.getFullYear()+pad2(d.getMonth()+1)+pad2(d.getDate());},
    heuresys:function(a,l){argc(a,0,0,'HeureSys',l);var d=new Date();return pad2(d.getHours())+pad2(d.getMinutes())+pad2(d.getSeconds())+pad2(Math.floor(d.getMilliseconds()/10));},
    dateverschaine:function(a,l){argc(a,1,2,'DateVersChaîne',l);var s=S(a[0]);return s.length>=8?s.slice(6,8)+'/'+s.slice(4,6)+'/'+s.slice(0,4):s;}
  };

  function eq(a,b){
    if(typeof a==='number'&&typeof b==='number')return a===b;
    if(typeof a==='boolean'||typeof b==='boolean')return truthy(a)===truthy(b);
    if((typeof a==='number'&&numLike(b))||(typeof b==='number'&&numLike(a)))return toNum(a)===toNum(b);
    return toStr(a)===toStr(b);
  }
  function cmp(a,b){
    if((typeof a==='number'||typeof a==='boolean')&&(typeof b==='number'||typeof b==='boolean'))return toNum(a)-toNum(b);
    if((typeof a==='number'&&numLike(b))||(typeof b==='number'&&numLike(a)))return toNum(a)-toNum(b);
    var x=toStr(a),y=toStr(b);return x<y?-1:x>y?1:0;
  }
  function soft(s){return toStr(s).trim().replace(/\s+/g,' ').toLowerCase();}
  function verySoft(s){return norm(toStr(s)).replace(/[^a-z0-9]/g,'');}

  function binop(o,a,b,line){
    switch(o){
      case '+':
        if(isTab(a)||isTab(b))fail('Un tableau ne peut pas être utilisé dans une addition.',line);
        if(typeof a==='string'||typeof b==='string')return toStr(a)+toStr(b);
        return toNum(a,line)+toNum(b,line);
      case '-':return toNum(a,line)-toNum(b,line);
      case '*':return toNum(a,line)*toNum(b,line);
      case '/':{var d=toNum(b,line);if(d===0)fail('Division par 0.',line);return toNum(a,line)/d;}
      case '%':{var m=toNum(b,line);if(m===0)fail('Modulo par 0.',line);return toNum(a,line)%m;}
      case '=':return eq(a,b);
      case '<>':return !eq(a,b);
      case '~=':return (typeof a==='string'||typeof b==='string')?soft(a)===soft(b):eq(a,b);
      case '~~':return (typeof a==='string'||typeof b==='string')?verySoft(a)===verySoft(b):eq(a,b);
      case '<':return cmp(a,b)<0;
      case '>':return cmp(a,b)>0;
      case '<=':return cmp(a,b)<=0;
      case '>=':return cmp(a,b)>=0;
    }
  }

  function unknown(name,line){fail('« '+name+' » est inconnu. Déclarez-le d\'abord, par exemple : '+name+' est un entier',line);}
  function index(base,iv,line){
    var i=Math.trunc(toNum(iv,line));
    if(isTab(base)){if(i<1||i>base.items.length)fail('Indice '+i+' hors du tableau ('+base.items.length+' élément'+(base.items.length>1?'s':'')+'). Les indices commencent à 1.',line);return i;}
    if(typeof base==='string'){if(i<1||i>base.length)fail('Indice '+i+' hors de la chaîne ('+base.length+' caractères).',line);return i;}
    fail('Seuls les tableaux et les chaînes s\'indicent avec [ ].',line);
  }

  function ev(e,sc){
    switch(e.type){
      case 'num':case 'str':case 'lit':return e.v;
      case 'arr':return newTab('variant',e.items.map(function(x){var v=ev(x,sc);return isTab(v)?copyTab(v,null,e.line):v;}));
      case 'var':{var r=sc.find(e.n);if(r)return r.v;if(Object.prototype.hasOwnProperty.call(CONSTS,e.n))return CONSTS[e.n];unknown(e.name,e.line);}
      case 'idx':{var b=ev(e.o,sc),i=index(b,ev(e.i,sc),e.line);return isTab(b)?b.items[i-1]:b.charAt(i-1);}
      case 'prop':{var o=ev(e.o,sc);if(e.p==='occurrence'){if(isTab(o))return o.items.length;fail('La propriété Occurrence s\'applique aux tableaux.',e.line);}if(e.p==='longueur'||e.p==='taille')return isTab(o)?o.items.length:toStr(o).length;fail('Propriété « '+e.name+' » non prise en charge dans le bac à sable.',e.line);}
      case 'un':{var v=ev(e.e,sc);return e.op==='-'?-toNum(v,e.line):!truthy(v);}
      case 'bin':{
        var a=ev(e.l,sc);
        if(e.op==='et')return truthy(a)&&truthy(ev(e.r,sc));
        if(e.op==='ou')return truthy(a)||truthy(ev(e.r,sc));
        return binop(e.op,a,ev(e.r,sc),e.line);
      }
      case 'call':return call(e,sc);
    }
    fail('Expression non prise en charge.',e.line);
  }

  function call(e,sc){
    var pr=procs[e.n],f=B[e.n];
    if(!pr&&!f){
      if(/^[hH][A-Z]/.test(e.name)||/^sql/.test(e.n))fail('« '+e.name+' » accède à une base de données : non disponible dans le bac à sable.',e.line);
      fail('Fonction inconnue : « '+e.name+' ».',e.line);
    }
    var args=e.args.map(function(x){return ev(x,sc);});
    if(pr){
      if(args.length>pr.params.length)fail(pr.name+' attend '+pr.params.length+' paramètre'+(pr.params.length>1?'s':'')+', '+args.length+' reçu'+(args.length>1?'s':'')+'.',e.line);
      if(++depth>MAX_DEPTH)fail('Trop d\'appels imbriqués : récursivité infinie ?',e.line);
      var local=new Scope(global);
      pr.params.forEach(function(prm,k){
        var v;
        if(k<args.length)v=args[k];
        else if(prm.def)v=ev(prm.def,global);
        else fail('Paramètre « '+prm.name+' » manquant dans l\'appel de '+pr.name+'.',e.line);
        var t=prm.ty?prm.ty.t:'variant';
        local.vars[norm(prm.name)]={t:t,elem:prm.ty?prm.ty.elem:'variant',v:t==='tab'?needTab(v,pr.name,e.line):coerce(t,v,e.line)};
      });
      try{run(pr.body,local);}
      catch(x){if(x instanceof Ret){depth--;return x.v;}if(x instanceof Brk||x instanceof Cont)fail('SORTIR ou CONTINUER en dehors d\'une boucle.',e.line);depth--;throw x;}
      depth--;return null;
    }
    return f(args,e.line);
  }

  function assign(target,value,sc,line){
    if(target.type==='var'){
      var r=sc.find(target.n);if(!r)unknown(target.name,line);
      if(r.t==='tab'){if(!isTab(value))fail('Un tableau est attendu pour « '+target.name+' ».',line);r.v=copyTab(value,r.elem,line);}
      else r.v=coerce(r.t,value,line);
      return;
    }
    var base=ev(target.o,sc);
    if(!isTab(base))fail('Seuls les éléments d\'un tableau peuvent être modifiés avec [ ].',line);
    var i=Math.trunc(toNum(ev(target.i,sc),line));
    if(i<1||i>base.items.length)fail('Indice '+i+' hors du tableau ('+base.items.length+' élément'+(base.items.length>1?'s':'')+'). Utilisez TableauAjoute pour agrandir le tableau.',line);
    base.items[i-1]=isTab(value)?copyTab(value,null,line):coerce(base.elem,value,line);
  }
  function current(target,sc){return ev(target,sc);}

  function declareLoopVar(sc,name,t){var n=norm(name),r=sc.find(n);if(!r){r={t:t,elem:'variant',v:defVal(t)};sc.vars[n]=r;}return r;}

  function loopBody(body,sc){try{run(body,sc);}catch(x){if(x instanceof Brk)return false;if(x instanceof Cont)return true;throw x;}return true;}

  function exec(s,sc){
    tick(s.line);
    switch(s.type){
      case 'nop':return;
      case 'decl':
        s.names.forEach(function(name){
          var n=norm(name);
          /* une déclaration placée dans une boucle est réexécutée : elle réinitialise la variable */
          if(Object.prototype.hasOwnProperty.call(sc.vars,n)&&sc.vars[n].decl!==s)fail('« '+name+' » est déjà déclaré.',s.line);
          var ty=s.ty,v;
          if(ty.t==='tab'){
            if(s.init){var iv=ev(s.init,sc);if(!isTab(iv))fail('Un tableau est attendu pour initialiser « '+name+' ».',s.line);v=copyTab(iv,ty.elem,s.line);}
            else{v=newTab(ty.elem);for(var k=0;k<ty.size;k++)v.items.push(defVal(ty.elem));}
          }else v=s.init?coerce(ty.t,ev(s.init,sc),s.line):defVal(ty.t);
          sc.vars[n]={t:ty.t,elem:ty.elem,v:v,decl:s};
        });
        return;
      case 'assign':{
        var val=ev(s.e,sc);
        if(s.op!=='='){var o=s.op.charAt(0);val=binop(o,current(s.target,sc),val,s.line);}
        assign(s.target,val,sc,s.line);return;
      }
      case 'expr':ev(s.e,sc);return;
      case 'if':
        if(truthy(ev(s.cond,sc)))run(s.then,sc);else if(s.els)run(s.els,sc);
        return;
      case 'while':
        while(truthy(ev(s.cond,sc))){tick(s.line);if(!loopBody(s.body,sc))break;}
        return;
      case 'loop':{
        var cnt=s.count?Math.trunc(toNum(ev(s.count,sc),s.line)):-1;
        for(var c=0;cnt<0||c<cnt;c++){tick(s.line);if(!loopBody(s.body,sc))break;}
        return;
      }
      case 'for':{
        var r=declareLoopVar(sc,s.v,'entier');
        var from=toNum(ev(s.from,sc),s.line),to=toNum(ev(s.to,sc),s.line),st=s.step?toNum(ev(s.step,sc),s.line):1;
        if(st===0)fail('Le PAS d\'une boucle POUR ne peut pas valoir 0.',s.line);
        r.v=coerce(r.t,from,s.line);
        while(st>0?r.v<=to:r.v>=to){tick(s.line);if(!loopBody(s.body,sc))break;r.v=coerce(r.t,toNum(r.v)+st,s.line);}
        return;
      }
      case 'foreach':{
        var tab=ev(s.e,sc);
        if(typeof tab==='string')tab=newTab('chaine',tab.split(''));
        if(!isTab(tab))fail('POUR TOUT attend un tableau.',s.line);
        var rv=declareLoopVar(sc,s.v,'variant'),ri=s.iv?declareLoopVar(sc,s.iv,'entier'):null;
        for(var i=0;i<tab.items.length;i++){tick(s.line);rv.v=coerce(rv.t,tab.items[i],s.line);if(ri)ri.v=i+1;if(!loopBody(s.body,sc))break;}
        return;
      }
      case 'forsplit':{
        var parts=toStr(ev(s.src,sc)).split(toStr(ev(s.sep,sc)));
        var rs=declareLoopVar(sc,s.v,'chaine');
        for(var j=0;j<parts.length;j++){tick(s.line);rs.v=coerce(rs.t,parts[j],s.line);if(!loopBody(s.body,sc))break;}
        return;
      }
      case 'switch':{
        var subj=ev(s.subj,sc);
        for(var k=0;k<s.cases.length;k++){
          var cs=s.cases[k],hit=cs.conds.some(function(cd){
            if(cd.lo)return cmp(subj,ev(cd.lo,sc))>=0&&cmp(subj,ev(cd.hi,sc))<=0;
            return binop(cd.op,subj,ev(cd.e,sc),s.line);
          });
          if(hit){run(cs.body,sc);return;}
        }
        if(s.def)run(s.def,sc);
        return;
      }
      case 'ret':throw new Ret(s.e?ev(s.e,sc):null);
      case 'brk':throw new Brk();
      case 'cont':throw new Cont();
    }
  }
  function run(body,sc){for(var i=0;i<body.length;i++)exec(body[i],sc);}

  try{
    var prog=parse(lex(source),procs);
    try{run(prog,global);}
    catch(x){if(x instanceof Ret)return {out:out,err:null,steps:steps};if(x instanceof Brk||x instanceof Cont)fail('SORTIR ou CONTINUER en dehors d\'une boucle.',null);throw x;}
    return {out:out,err:null,steps:steps};
  }catch(x){
    if(x instanceof Err)return {out:out,err:(x.line?'Ligne '+x.line+' : ':'')+x.message,steps:steps};
    if(x instanceof RangeError)return {out:out,err:'Exécution interrompue : pile d\'appels saturée.',steps:steps};
    throw x;
  }
};
})();
