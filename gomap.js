// When locator icon in datatable is clicked, go to that spot on the map
$(document).on("click", ".go-map", function(e) {
  e.preventDefault();
  $el = $(this);
  var lat = $el.data("lat");
  var long = $el.data("long");
  var layerid = $el.data("layerid");
  $($("#shiny-tab-nav")[0]).tab("show");
  Shiny.setInputValue("goto", {
    lat: lat,
    lng: long,
    layerid: layerid,
    nonce: Math.random()
  });
});


//<a href="#shiny-tab-nav" data-toggle="tab" data-value="nav" aria-expanded="true" tabindex="0" aria-selected="true">
//              <span>Dashboard</span>
//            </a>