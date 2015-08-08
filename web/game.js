$(function ($) {
    $('body').keyup(function (event) {
        console.log('keyup');
        $.post('/move.svg', { move: event.key }, function () {
            alert('success');
        }).fail(function () {
            alert('error');
        });
    });
});
