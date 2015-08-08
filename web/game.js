$(function ($) {
    var body = $('body');
    body.keyup(function (event) {
        $.post('/move.svg', { move: event.key }, function (data) {
            body.empty();
            body.append(data);
        }).fail(function () {
            alert('error');
        });
    });
});
