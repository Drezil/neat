/*
neat utility functions
*/

Number.prototype.formatMoney = function(c, d, t) {
var n = this,
    c = isNaN(c = Math.abs(c)) ? 2 : c,
    d = d == undefined ? "." : t,
    t = t == undefined ? "," : d,
    s = n < 0 ? "-" : "",
    i = parseInt(n = Math.abs(+n || 0).toFixed(c)) + "",
    j = (j = i.length) > 3 ? j % 3 : 0;
   return s + (j ? i.substr(0, j) + t : "") + i.substr(j).replace(/(\d{3})(?=\d)/g, "$1" + t) + (c ? d + Math.abs(n - i).toFixed(c).slice(2) : "");
};


function checkOrders() {
  $('table.sellOrders tr.order').each(function(ix,el) {
    var d = $(el).attr('data').split(';');
    $.getJSON("https://public-crest.eveonline.com/market/"+d[1]+"/orders/sell/?type=https://public-crest.eveonline.com/types/"+d[0]+"/",function(ret, status, xhr) {
         var outbid = false;
         var outbidprice = parseFloat(d[2])/100;
         $(ret.items).each(function(index,order) {
           if (order.price < parseFloat(d[2])/100) {
             outbid = true;
             if (order.price < outbidprice) {
               outbidprice = order.price;
             }
           }
         });
         if (outbid) {
           $(el).addClass('outbid');
           var priceel = $(el).find('.price');
           priceel.html(function(index, old) {
             return old + "<br />" + outbidprice.formatMoney(2);
           });
         } else {
           $(el).addClass('not_outbid');
         }
    });
  });

  $('table.buyOrders tr.order').each(function(ix,el) {
    var d = $(el).attr('data').split(';');
    $.getJSON("https://public-crest.eveonline.com/market/"+d[1]+"/orders/buy/?type=https://public-crest.eveonline.com/types/"+d[0]+"/",function(ret, status, xhr) {
         var outbid = false;
         var outbidprice = parseFloat(d[2])/100;
         $(ret.items).each(function(index,order) {
           if (order.price > parseFloat(d[2])/100) {
             outbid = true;
             if (order.price > outbidprice) {
               outbidprice = order.price;
             }
           }
         });
         if (outbid) {
           $(el).addClass('outbid');
           var priceel = $(el).find('.price');
           priceel.html(function(index, old) {
             return old + "<br />" + outbidprice.formatMoney(2);
           });
         } else {
           $(el).addClass('not_outbid');
         }
    });
  });

}

