$(document).ready(function() {
  console.log('check');
  $(".main-header").prepend("<span id='titleHeader'>Welcome to the Soundburst Application</span>");

  $("#speciesDropSubmit").click(function() {
    var count = $(".completedBrush").length;
    $("#spectrogram_brush").clone(true).prop("id", count + 1).prop("class", "completedBrush").css({'background-color':'green'}).appendTo("#spectrogram");
  });

  $(document).on('click', '.removeAnn', function(e) {
    debugger;
    var id = $(this).parent()[0].id;
    $("#spectrogram #" + id).remove();
    $("#listCompleted #" + id).remove();
    var annotationNumber = id.substr(id.length - 1);
    //var out = console.r.call('removeAnnotationFromCSV', {annotationNumber: annotationNumber});
    //perform the request
    var req = ocpu.rpc("removeAnnotationFromCSV", {
      annotationNumber : annotationNumber
    }, function(output){
      console.log("it works!");
    });

    req.fail(function(){
      alert("Server error: " + req.responseText);
    });

    //after request complete, re-enable the button
    req.always(function(){
      alert("works: " + req.responseText);
    });
  });

});
