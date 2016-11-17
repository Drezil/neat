/*
neat utility functions
*/

Number.prototype.formatMoney = function(c, d, t) {
var n = this,
    c = isNaN(c = Math.abs(c)) ? 2 : c,
    d = d == undefined ? "." : d,
    t = t == undefined ? "," : t,
    s = n < 0 ? "-" : "",
    i = parseInt(n = Math.abs(+n || 0).toFixed(c)) + "",
    j = (j = i.length) > 3 ? j % 3 : 0;
   return s + (j ? i.substr(0, j) + t : "") + i.substr(j).replace(/(\d{3})(?=\d)/g, "$1" + t) + (c ? d + Math.abs(n - i).toFixed(c).slice(2) : "");
};


function checkOrders() {
  $('table.sellOrders tr.order').each(function(ix,el) {
    var d = $(el).attr('data').split(';');
    $.getJSON("https://esi.tech.ccp.is/latest/markets/"+d[1]+"/orders/?type_id="+d[0]+"&order_type=sell&datasource=tranquility",function(ret, status, xhr) {
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
    $.getJSON("https://esi.tech.ccp.is/latest/markets/"+d[1]+"/orders/?type_id="+d[0]+"&order_type=buy&datasource=tranquility",function(ret, status, xhr) {
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

function sortTableBy(tblSel,col,des,desc) {
  var rows = $(tblSel + ' > tbody > tr');
  rows.detach();
  rows = rows.sort(function(a,b) {
    fst = $($(a).children()[col]);
    snd = $($(b).children()[col]);
    res = 0;
    if (fst.hasClass('numeric')) {
      res =  parseFloat(fst.text().replace(/\./g,"").replace(",",".")) 
           - parseFloat(snd.text().replace(/\./g,"").replace(",","."));
    } else {
      if (fst.text() == snd.text())
        res = $($(a).children()[des]).text() < $($(b).children()[des]).text() ? -1 : 1;
      else
        res = fst.text() < snd.text() ? -1 : 1;
    }
    if (desc)
      return res;
    else
      return -1*res;
  });
  rows.appendTo($(tblSel + ' > tbody'));
}

function hideTableCol(tblSel, col, resetGroup) {
  var desc = $($(tblSel + ' > thead > tr').children()[col]).text();
  var rows = $(tblSel + ' > * > tr');
  rows.each(function(id,elem) {
    $($(elem).children()[col]).fadeOut();
  });
  var restore = $('<a class="btn" role="button">Show '+desc+'</a>');
  restore.on('click', function() {
    showTableCol(tblSel,col,restore);
  });
  $(resetGroup).append(restore);
}

function showTableCol(tblSel, col, resetButton) {
  var rows = $(tblSel + ' > * > tr');
  rows.each(function(id,elem) {
    $($(elem).children()[col]).fadeIn();
  });
  $(resetButton).remove();
}

