/*
  Copyright © 2009 Stéphane Glondu <steph@glondu.net>
  Copyright © 2009 Mehdi Dogguy <dogguy@pps.jussieu.fr>

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Dependencies: jquery.
*/

function update () {
    if ($("#good").is(":checked")) {
        $(".src").filter(".good").parent().show();
    } else {
        $(".src").filter(".good").parent().hide();
    }
    if ($("#bad").is(":checked")) {
        $(".src").filter(".bad").parent().show();
    } else {
        $(".src").filter(".bad").parent().hide();
    }
    if ($("#unknown").is(":checked")) {
        $(".src").filter(".unknown").parent().show();
    } else {
        $(".src").filter(".unknown").parent().hide();
    }
    if ($("#hide_all_ok").is(":checked")) {
        $(".all_ok").parent().hide();
    }
    for (i = 0; i < nb_rounds; i++) {
        if ($(".round"+i).filter(":visible").length > 0) {
            $("#header"+i).show();
        } else {
            $("#header"+i).hide();
        }
    }
    $("#count").html(" ("+$(".srcname").filter(":visible").length+")");
};

function init () {
    $("#good").click(update);
    $("#bad").click(update);
    $("#unknown").click(update);
    $("#hide_all_ok").click(update);
    update();
}

$(document).ready(init);
