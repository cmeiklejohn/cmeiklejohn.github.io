/* Decorative network traffic. No data collection or external dependencies. */
(()=>{
 const canvas=document.querySelector('.pitch-network'),button=document.querySelector('#pitch-motion');
 if(!canvas||!button)return;
 const ctx=canvas.getContext('2d');if(!ctx)return;
 const reduced=matchMedia('(prefers-reduced-motion: reduce)');
 let paused=reduced.matches,w=0,h=0,frame=0,raf=0;
 const paths=Array.from({length:9},(_,i)=>({phase:i*.73,y:(i+.5)/9,speed:.000025+(i%3)*.000009}));
 function resize(){w=innerWidth;h=innerHeight;const d=Math.min(devicePixelRatio||1,2);canvas.width=w*d;canvas.height=h*d;ctx.setTransform(d,0,0,d,0,0);draw(performance.now());}
 function point(p,t){return{x:t*w,y:p.y*h+Math.sin(t*Math.PI*2+p.phase)*Math.min(115,h*.14)}}
 function draw(time){ctx.clearRect(0,0,w,h);const dark=document.documentElement.dataset.theme==='dark'||(!document.documentElement.dataset.theme&&matchMedia('(prefers-color-scheme:dark)').matches);
  ctx.lineWidth=1;ctx.strokeStyle=dark?'rgba(109,224,246,.14)':'rgba(80,84,147,.13)';
  for(const p of paths){ctx.beginPath();for(let i=0;i<=60;i++){const q=point(p,i/60);i?ctx.lineTo(q.x,q.y):ctx.moveTo(q.x,q.y)}ctx.stroke();
   for(let n=0;n<3;n++){const t=((paused?frame:time)*p.speed+n/3+p.phase)%1,q=point(p,t);ctx.fillStyle=dark?'rgba(109,224,246,.45)':'rgba(80,84,147,.28)';ctx.fillRect(Math.round(q.x),Math.round(q.y),3,3)}
  }
 }
 function tick(t){frame=t;draw(t);if(!paused&&!document.hidden)raf=requestAnimationFrame(tick)}
 function update(){cancelAnimationFrame(raf);button.textContent=paused?'Animate background':'Pause background';button.setAttribute('aria-pressed',String(paused));if(paused)draw(frame);else raf=requestAnimationFrame(tick)}
 button.addEventListener('click',()=>{paused=!paused;update()});
 reduced.addEventListener('change',()=>{paused=reduced.matches;update()});
 document.addEventListener('visibilitychange',()=>{cancelAnimationFrame(raf);if(!document.hidden)update()});
 addEventListener('resize',resize,{passive:true});resize();update();
})();
