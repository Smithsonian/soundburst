$(document).ready(function() {
  $(".main-header").prepend("<span id='titleHeader'>Welcome to the Soundburst Application</span>");

  $("#speciesDropSubmit").click(function() {
    var count = $(".completedBrush").length;
    $("#spectrogram_brush").clone(true).prop("id", count + 1).prop("class", "completedBrush").css({'background-color':'green'}).appendTo("#spectrogram")
  });

  $(document).on('click', '.removeAnn', function(e) {
    var id = $(this).parent()[0].id;
    $("#spectrogram #clip" + id).remove();
    $("#listCompleted #clip" + id).remove();
  });

});
