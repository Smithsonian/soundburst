$(document).ready(function() {
  $(".main-header").prepend("<span id='titleHeader'>Welcome to the Soundburst Application</span>");

  $("#speciesDropSubmit").click(function() {
    console.log("test");
    var count = $(".completedBrush").length;
    $("#spectrogram_brush").clone(true).prop("id", count + 1).prop("class", "completedBrush").css({'background-color':'green'}).appendTo("#spectrogram")
  });
  //
  // var left = $("#test").css("left");
  // var top = $("#test").css("top");
  // var height = $("#test").css("height");
  // var width = $("#test").css("width");
  // var borderright = $("#test").css("border-right");
  // var borderleft = $("#test").css("border-left");
  //
  // $("#spectrogram_brush").css("left", left);
  // $("#spectrogram_brush").css("top", top);
  // $("#spectrogram_brush").css("height", height);
  // $("#spectrogram_brush").css("width", width);
  // $("#spectrogram_brush").css("border-right", borderright);
  // $("#spectrogram_brush").css("border-left", borderleft);
  // $("#test").remove()
});
