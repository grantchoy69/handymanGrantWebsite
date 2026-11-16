const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');
const assert = require('node:assert/strict');
const crypto = require('node:crypto');
const root = path.resolve(__dirname, '..');
const source = fs.readFileSync(path.join(root, 'quote-intake.js'), 'utf8');
const flush = async () => { for (let i = 0; i < 4; i++) await new Promise(setImmediate); };
function element() { return {dataset:{},style:{},attributes:{},textContent:'',disabled:false,setAttribute(k,v){this.attributes[k]=v;},removeAttribute(k){delete this.attributes[k];}}; }
function harness(options={}) {
  const status=element(), button=element(), honeypot=element(), calls=[], analytics=[];
  button.textContent='Send Request';
  class Form {
    constructor(){this.dataset={};this.handlers=[];this.resetCount=0;this.fields={name:'Synthetic test',phone:'6195550100',email:'test@example.com',zip:'92104',message:'Test the quote request flow.',website:''};}
    querySelector(s){return s.includes('honeypot')?honeypot:s.includes('form-status')?status:s.includes('submit')?button:null;}
    addEventListener(type,fn){if(type==='submit')this.handlers.push(fn);}
    reset(){this.resetCount++;this.fields={};}
    submit(){for(const fn of this.handlers)fn({preventDefault(){}});}
  }
  const form=new Form(); let saved=options.storageFailure==='removeItem'?JSON.stringify({savedAt:'2000-01-01'}):null;
  const store={getItem(){if(options.storageFailure==='getItem')throw Error('blocked');return saved;},setItem(k,v){if(options.storageFailure==='setItem')throw Error('blocked');saved=v;},removeItem(){if(options.storageFailure==='removeItem')throw Error('blocked');saved=null;}};
  const search=options.search??'?gclid=TEST&utm_source=google&utm_medium=cpc';
  const window={location:{href:'https://handymangrant.com/'+search,pathname:'/',search},crypto,setTimeout(...args){const timer=setTimeout(...args);timer.unref();return timer;},clearTimeout,gtag(...args){analytics.push(args);if(options.analyticsThrows)throw Error('analytics blocked');}};
  Object.defineProperty(window,'localStorage',{get(){if(options.storageFailure==='getter')throw Error('blocked');return store;}});
  const document={readyState:'complete',referrer:options.referrer??'',title:'Handyman Grant',querySelectorAll(s){return s==='a[href]'?[]:[form];},createElement(){throw Error('Unexpected helper creation');}};
  const fetch=async(url,init)=>{const payload=JSON.parse(init.body);calls.push(payload);if(options.onFetch)return options.onFetch(payload,calls.length);return {ok:true,json:async()=>({success:true,transactionId:payload.transactionId})};};
  vm.runInNewContext(source,{window,document,navigator:{language:'en-US'},HTMLFormElement:Form,FormData:class{constructor(f){this.fields={...f.fields};}get(k){return this.fields[k]??null;}},fetch,AbortController,URLSearchParams,Uint8Array,console});
  assert.equal(form.handlers.length,1);
  return {form,status,button,calls,analytics};
}
const conversions=h=>h.analytics.filter(x=>x[1]==='conversion');
function accepted(h){assert.equal(h.status.dataset.state,'success');assert.equal(h.form.resetCount,1);assert.equal(h.form.dataset.hgPendingTransactionId,undefined);assert.equal(h.button.disabled,false);}
(async()=>{
  for(const storageFailure of ['getter','getItem','setItem','removeItem']){const h=harness({storageFailure});h.form.submit();await flush();accepted(h);assert.equal(conversions(h).length,1);}
  const h=harness();h.form.submit();await flush();accepted(h);assert.equal(conversions(h).length,1);assert.equal(conversions(h)[0][2].transaction_id,h.calls[0].transactionId);assert.equal(h.calls[0].gclid,'TEST');
  const retry=harness({onFetch(p,n){return n===1?Promise.reject(Error('network')):Promise.resolve({ok:true,json:async()=>({success:true,transactionId:p.transactionId})});}});
  retry.form.submit();await flush();assert.equal(retry.form.resetCount,0);assert.equal(conversions(retry).length,0);retry.form.submit();await flush();accepted(retry);assert.equal(retry.calls[0].transactionId,retry.calls[1].transactionId);assert.equal(conversions(retry).length,1);
  let finish;const pending=harness({onFetch(p){return new Promise(resolve=>{finish=()=>resolve({ok:true,json:async()=>({success:true,transactionId:p.transactionId})});});}});
  pending.form.submit();pending.form.submit();assert.equal(pending.calls.length,1);finish();await flush();accepted(pending);
  const analytics=harness({analyticsThrows:true});analytics.form.submit();await flush();accepted(analytics);
  const invalid=harness();invalid.form.fields.name='';invalid.form.submit();await flush();assert.equal(invalid.calls.length,0);assert.equal(invalid.status.dataset.state,'error');
  for(const [search,expected] of [['?utm_source=google','organic'],['?utm_source=google&utm_medium=cpc','googleAds'],['?utm_source=chatgpt.com','chatgpt'],['?wbraid=TEST','googleAds']]){const test=harness({search});test.form.submit();await flush();accepted(test);assert.equal(test.analytics.find(x=>x[1]==='generate_lead')[2].lead_source,expected);}
  const pages=['index.html','work.html','landing-page-template.html',...fs.readdirSync(path.join(root,'landing-pages')).filter(x=>x.endsWith('.html')).map(x=>'landing-pages/'+x)];
  assert.equal(pages.length,10);let forms=0;
  for(const file of pages){const html=fs.readFileSync(path.join(root,file),'utf8');assert.equal((html.match(/quote-intake\.js/g)||[]).length,1,file);assert.match(html,/gtag\(['"]config['"],\s*['"]G-65Z3SGSQP3['"]\)/,file+' GA4 preserved');
    const anchors=html.match(/<a\b[^>]*>/gi)||[];let trackedPhones=0;
    for(const anchor of anchors){const href=anchor.match(/\bhref\s*=\s*(["'])(.*?)\1/i)?.[2]||'';if(href==='#contact'||href.endsWith('index.html#contact')||href.endsWith('/#contact'))assert.ok(!anchor.includes('gtag_report_conversion'),file+' navigation conversion');if(href.startsWith('tel:')&&anchor.includes('gtag_report_conversion'))trackedPhones++;}
    assert.equal(trackedPhones,file==='landing-page-template.html'?2:3,file+' phone tracking');
    if(file==='index.html'||file.startsWith('landing-pages/')){assert.equal((html.match(/data-hg-quote-form/g)||[]).length,1,file);forms++;}
  }
  assert.equal(forms,8);
  console.log('PASS: blocked storage, accepted conversions, failed/retried submissions, concurrent submit guard, analytics failure, validation, paid/organic/ChatGPT attribution; 10 pages/8 forms, GA4 and phone tracking preserved.');
})().catch(error=>{console.error(error);process.exitCode=1;});
