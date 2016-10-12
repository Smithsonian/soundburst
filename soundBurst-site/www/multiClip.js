(function() {
  console.log('check');
  jQuery(".main-header").prepend("<span id='titleHeader'>Welcome to the Soundburst Application</span>");

  //jQuery("#speciesDropSubmit").click(function() {
  jQuery(document).on('click', '#speciesDropSubmit', function(e) {
    var count = jQuery(".completedBrush").length;
    jQuery("#spectrogram_brush").clone(true).prop("id", count + 1).prop("class", "completedBrush").css({'background-color':'green'}).appendTo("#spectrogram");
  });

  jQuery(document).on('click', '.removeAnn', function(e) {
    var id = jQuery(this).parent()[0].id;
    jQuery("#spectrogram #" + id).remove();
    jQuery("#listCompleted #" + id).remove();
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

})(jQuery);
