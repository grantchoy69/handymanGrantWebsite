const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');
const assert = require('node:assert/strict');
const crypto = require('node:crypto');
const root = path.resolve(__dirname, '..');
const source = fs.readFileSync(path.join(root, 'quote-intake.js'), 'utf8');
const flush = async () => { for (let i = 0; i < 4; i++) await new Promise(setImmediate); };
function element(tag='div') {
  let text='';
  return {tag,dataset:{},style:{},attributes:{},disabled:false,hidden:false,value:'',files:[],children:[],handlers:{},
    get textContent(){return text;},set textContent(v){text=v;this.children=[];},
    setAttribute(k,v){this.attributes[k]=v;},removeAttribute(k){delete this.attributes[k];},
    appendChild(child){this.children.push(child);return child;},
    addEventListener(type,handler){(this.handlers[type]||=[]).push(handler);},
    emit(type,event={}){for(const handler of this.handlers[type]||[])handler(event);},
    querySelectorAll(name){return this.children.flatMap(child=>[...(child.tag===name?[child]:[]),...child.querySelectorAll(name)]);}};
}
function harness(options={}) {
  const status=element(), button=element('button'), honeypot=element(), input=element('input'), list=element('ul'), count=element(), calls=[], analytics=[],requests=[],created=[],revoked=[],timeouts=[],windowHandlers={};
  button.textContent='Send Request';
  class Form {
    constructor(){this.dataset={};this.handlers=[];this.resetCount=0;this.fields={name:'Synthetic test',phone:'6195550100',email:'test@example.com',zip:'92104',message:'Test the quote request flow.',website:''};}
    querySelector(s){return s.includes('honeypot')?honeypot:s.includes('form-status')?status:s.includes('photo-input')?input:s.includes('photo-list')?list:s.includes('photo-count')?count:s.includes('submit')?button:null;}
    addEventListener(type,fn){if(type==='submit')this.handlers.push(fn);}
    reset(){this.resetCount++;this.fields={};}
    submit(){for(const fn of this.handlers)fn({preventDefault(){}});}
  }
  const form=new Form(); let saved=options.storageFailure==='removeItem'?JSON.stringify({savedAt:'2000-01-01'}):null;
  const store={getItem(){if(options.storageFailure==='getItem')throw Error('blocked');return saved;},setItem(k,v){if(options.storageFailure==='setItem')throw Error('blocked');saved=v;},removeItem(){if(options.storageFailure==='removeItem')throw Error('blocked');saved=null;}};
  const search=options.search??'?gclid=TEST&utm_source=google&utm_medium=cpc';
  const window={location:{href:'https://handymangrant.com/'+search,pathname:'/',search},crypto,setTimeout(fn,ms){timeouts.push(ms);const timer=setTimeout(fn,ms);timer.unref();return timer;},clearTimeout,gtag(...args){analytics.push(args);if(options.analyticsThrows)throw Error('analytics blocked');},URL:{createObjectURL(photo){const url='blob:test-'+created.length;created.push({photo,url});return url;},revokeObjectURL(url){revoked.push(url);}},addEventListener(type,handler){(windowHandlers[type]||=[]).push(handler);}};
  Object.defineProperty(window,'localStorage',{get(){if(options.storageFailure==='getter')throw Error('blocked');return store;}});
  class FakeFormData{constructor(f){this.fields={...f?.fields};this.parts=[];}get(k){return this.parts.find(part=>part[0]===k)?.[1]??this.fields[k]??null;}getAll(k){return this.parts.filter(part=>part[0]===k).map(part=>part[1]);}append(k,value,name){this.parts.push([k,value,name]);}}
  const document={readyState:'complete',referrer:options.referrer??'',title:'Handyman Grant',querySelectorAll(s){return s==='a[href]'?[]:[form];},createElement:element};
  const fetch=async(url,init)=>{const multipart=init.body instanceof FakeFormData,payload=JSON.parse(multipart?init.body.get('payload'):init.body),photos=multipart?init.body.getAll('photos'):[];calls.push(payload);requests.push({...init,payload,photos,multipart});if(options.onFetch)return options.onFetch(payload,calls.length,photos);return {ok:true,json:async()=>({success:true,transactionId:payload.transactionId,photoCount:photos.length})};};
  vm.runInNewContext(source,{window,document,navigator:{language:'en-US'},HTMLFormElement:Form,FormData:FakeFormData,fetch,AbortController,URLSearchParams,Uint8Array,console});
  assert.equal(form.handlers.length,1);
  return {form,status,button,calls,analytics,input,list,count,requests,created,revoked,timeouts,select(photos){input.files=photos;input.emit('change');},leave(persisted=false){for(const handler of windowHandlers.pagehide||[])handler({persisted});}};
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
  assert.equal(h.requests[0].multipart,false);assert.equal(h.requests[0].headers['Content-Type'],'application/json');assert.equal(h.timeouts[0],15000);
  const MiB=1024*1024,photo=(name,type='image/jpeg',size=1024)=>({name,type,size,lastModified:1}),jpg=photo('one.jpg'),png=photo('two.png','image/png');
  const selection=harness();selection.select([jpg]);selection.select([jpg,png]);assert.equal(selection.list.children.length,2);
  const remove=selection.list.querySelectorAll('button')[0];remove.emit('click');remove.emit('click');assert.equal(selection.list.children.length,1);assert.equal(selection.revoked.length,1);
  selection.select([photo('bad.gif','image/gif')]);assert.equal(selection.list.children.length,1);assert.match(selection.status.textContent,/JPG/);
  selection.select([photo('large.jpg','image/jpeg',5*MiB+1)]);assert.equal(selection.list.children.length,1);assert.match(selection.status.textContent,/5 MB/);
  const tooMany=harness();tooMany.select(Array.from({length:6},(_,i)=>photo(i+'.jpg')));assert.equal(tooMany.list.children.length,0);assert.match(tooMany.status.textContent,/5 photos/);
  const tooBig=harness();tooBig.select(Array.from({length:5},(_,i)=>photo(i+'.jpg','image/jpeg',i===4?MiB:5*MiB)));assert.equal(tooBig.list.children.length,0);assert.match(tooBig.status.textContent,/20 MB/);
  const boundary=harness();boundary.select(Array.from({length:5},(_,i)=>photo(i+'.jpg','image/jpeg',4*MiB)));assert.equal(boundary.list.children.length,5);
  const heic=harness();heic.select([photo('iphone.heic','image/heic')]);const preview=heic.list.querySelectorAll('img')[0];preview.emit('error');assert.equal(preview.hidden,true);assert.equal(heic.list.children[0].children[0].hidden,false);assert.equal(heic.revoked.length,1);
  const upload=harness();upload.select([jpg,png]);upload.form.submit();await flush();accepted(upload);assert.equal(upload.requests[0].multipart,true);assert.equal(upload.requests[0].headers['Content-Type'],undefined);assert.equal(upload.requests[0].photos[0],jpg);assert.equal(upload.requests[0].photos[1],png);assert.equal(upload.timeouts[0],90000);assert.equal(upload.list.children.length,0);assert.equal(upload.revoked.length,2);assert.equal(conversions(upload).length,1);
  const partial=harness({onFetch(p,n){return {ok:true,json:async()=>({success:true,transactionId:p.transactionId,photoCount:n===1?1:2})};}});partial.select([jpg,png]);partial.form.submit();await flush();assert.equal(partial.form.resetCount,0);assert.equal(partial.list.children.length,2);assert.equal(conversions(partial).length,0);assert.equal(partial.input.disabled,false);assert.match(partial.status.textContent,/not all confirmed/);const pendingPhotoId=partial.form.dataset.hgPendingTransactionId;partial.form.submit();await flush();accepted(partial);assert.equal(partial.calls[1].transactionId,pendingPhotoId);assert.equal(partial.requests[1].photos[0],jpg);assert.equal(conversions(partial).length,1);
  const missingCount=harness({onFetch(p){return {ok:true,json:async()=>({success:true,transactionId:p.transactionId})};}});missingCount.select([jpg]);missingCount.form.submit();await flush();assert.equal(missingCount.form.resetCount,0);assert.equal(missingCount.list.children.length,1);assert.equal(conversions(missingCount).length,0);
  const conflict=harness({onFetch(){return {ok:false,json:async()=>({error:'This transaction has different request details.'})};}});conflict.select([jpg]);conflict.form.submit();await flush();assert.equal(conflict.form.resetCount,0);assert.equal(conflict.list.children.length,1);assert.equal(conversions(conflict).length,0);assert.match(conflict.status.textContent,/different request details/);
  let finishUpload;const busy=harness({onFetch(p,n,photos){return new Promise(resolve=>{finishUpload=()=>resolve({ok:true,json:async()=>({success:true,transactionId:p.transactionId,photoCount:photos.length})});});}});busy.select([jpg]);busy.form.submit();busy.form.submit();assert.equal(busy.calls.length,1);assert.equal(busy.input.disabled,true);const busyRemove=busy.list.querySelectorAll('button')[0];assert.equal(busyRemove.disabled,true);busyRemove.emit('click');busy.select([png]);assert.equal(busy.list.children.length,1);finishUpload();await flush();accepted(busy);
  const leaving=harness();leaving.select([jpg,png]);leaving.leave(true);assert.equal(leaving.revoked.length,0);leaving.leave();assert.equal(leaving.revoked.length,2);
  const pages=['index.html','work.html','landing-page-template.html',...fs.readdirSync(path.join(root,'landing-pages')).filter(x=>x.endsWith('.html')).map(x=>'landing-pages/'+x)];
  assert.equal(pages.length,10);let forms=0;
  for(const file of pages){const html=fs.readFileSync(path.join(root,file),'utf8');assert.equal((html.match(/quote-intake\.js/g)||[]).length,1,file);assert.match(html,/gtag\(['"]config['"],\s*['"]G-65Z3SGSQP3['"]\)/,file+' GA4 preserved');
    const anchors=html.match(/<a\b[^>]*>/gi)||[];let trackedPhones=0;
    for(const anchor of anchors){const href=anchor.match(/\bhref\s*=\s*(["'])(.*?)\1/i)?.[2]||'';if(href==='#contact'||href.endsWith('index.html#contact')||href.endsWith('/#contact'))assert.ok(!anchor.includes('gtag_report_conversion'),file+' navigation conversion');if(href.startsWith('tel:')&&anchor.includes('gtag_report_conversion'))trackedPhones++;}
    assert.equal(trackedPhones,file==='landing-page-template.html'?2:3,file+' phone tracking');
    if(file==='index.html'||file.startsWith('landing-pages/')){assert.equal((html.match(/data-hg-quote-form/g)||[]).length,1,file);const inputTag=html.match(/<input\b[^>]*data-hg-photo-input[^>]*>/)?.[0]||'';assert.match(inputTag,/type="file"/);assert.match(inputTag,/\bmultiple\b/);assert.match(inputTag,/\bdisabled\b/);assert.match(inputTag,/image\/heic/);assert.match(html,/data-hg-photo-list/);assert.match(html,/20 MB combined/);assert.match(html,/<noscript>/);forms++;}
  }
  assert.equal(forms,8);
  console.log('PASS: existing intake behavior; photo selection/limits/previews/removal; multipart success, partial failure, retry, conflict, busy guards and cleanup; 10 pages/8 forms with GA4 and phone tracking.');
})().catch(error=>{console.error(error);process.exitCode=1;});
