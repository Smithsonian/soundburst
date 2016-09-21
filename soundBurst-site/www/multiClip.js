$(document).ready(function() {
  console.log("it's working")

  $("#speciesDropSubmit").click(function() {
    console.log("test");
    $("#spectrogram_brush").clone().append('#spectrogram');
  });

});
