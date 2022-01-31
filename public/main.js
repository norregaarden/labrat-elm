// https://www.elm-spa.dev/examples/03-storage
const app = Elm.Main.init({
  flags: JSON.parse(localStorage.getItem('storage'))
})

app.ports.save.subscribe(storage => {
  localStorage.setItem('storage', JSON.stringify(storage))
  app.ports.load.send(storage)
})

app.ports.blink.subscribe(blink => {
  console.log("BLINK")
  console.log(blink)

  // https://developer.mozilla.org/en-US/docs/Web/API/Web_Animations_API/Using_the_Web_Animations_API
  var blinkAnimation = [
    { opacity: '1.0'},
    { opacity: '1.0'}
  ];

  var blinkTiming = {
    duration: blink.duration,
    iterations: 1
  }


  window.setTimeout(function(){
      console.log("NOW")
      document.getElementById("blinkImage").animate(
        blinkAnimation,
        blinkTiming
      )
      console.log("now")
  }, 666)
})