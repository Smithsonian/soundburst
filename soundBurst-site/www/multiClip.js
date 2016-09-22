$(document).ready(function() {
  $(".main-header").prepend("<span id='titleHeader'>Welcome to the Soundburst Application</span>");

  $("#speciesDropSubmit").click(function() {
    console.log("test");
    $("#spectrogram_brush").clone().append('#spectrogram');
  });
});
