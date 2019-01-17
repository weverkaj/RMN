/*
 * jQuery v1.9.1 included
 */

$(document).ready(function() {

  //show "submit a request" link for paying customers
    (function() {
    var isCust;
    // find the tag in the array
    function isCustomer(element, index, array) {
      return (element === 'paying');
    }
    //go through the HelpCenter object and look for org tags
    HelpCenter.user.organizations.forEach(function(x) {
      isCust = x.tags.some(isCustomer);
      return (isCust === true);
    });
    //is this a customer and show them
    if (isCust === true) {
      $('a[href$="requests/new"]').show();
      $('.my-activities').show();
      $('#user-menu .my-activities').show();
      $('a.submit-a-request').show();
    }
  }());
  
  // social share popups
  $(".share a").click(function(e) {
    e.preventDefault();
    window.open(this.href, "", "height = 500, width = 500");
  });

  // toggle the share dropdown in communities
  $(".share-label").on("click", function(e) {
    e.stopPropagation();
    var isSelected = this.getAttribute("aria-selected") == "true";
    this.setAttribute("aria-selected", !isSelected);
    $(".share-label").not(this).attr("aria-selected", "false");
  });

  $(document).on("click", function() {
    $(".share-label").attr("aria-selected", "false");
  });
  
  //modify Submit a Request language on header
  $('nav.user-nav a.submit-a-request').html('Premium support');
  
  //language on Submit a Request page
  $('h1:contains(Submit a request)').html('Open a premium support ticket');
  
$('p:contains(Please enter the details of your request. A member of our support staff will respond as soon as possible.)').html('For premium support customers, please enter the details of your request. Open-source users should instead use our community forums for questions and suggestions.');

  // show form controls when the textarea receives focus
  $(".answer-body textarea").one("focus", function() {
    $(".answer-form-controls").show();
  });

  $(".comment-container textarea").one("focus", function() {
    $(".comment-form-controls").show();
  });

});

  // MW-Notification Banner
   $.get( "/api/v2/help_center/articles.json?label_names=alert" ).done(function( data ) {
     
   $.each(data.articles, function(index,item) {
     
     var style1 = '<div class="ns-box ns-bar ns-effect-slidetop ns-type-notice ns-show"><div class="ns-box-inner"><span class="megaphone"></span></i><p><a href="'+ item.html_url + '">' + item.title + '</a>' + item.body + '</p></div><span class="ns-close"></span></div>'
           
     $('.alertbox').append(style1);
   });
   $('.ns-close').on('click',function(){
    $(".alertbox").remove();
  });
    
  });