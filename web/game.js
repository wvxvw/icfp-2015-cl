$(function ($) {
    var body = $('body'),
        keys = { "65": "e", "70": "w", "83": "se", "68": "sw", "37": "cw", "39": "ccw" };
    body.keyup(function (event) {
        console.log('key: ' + event.keyCode);
        // 65 - a
        // 70 - f
        // 83 - s
        // 68 - d
        // 37 - left
        // 39 - right
        if ([65, 70, 83, 68, 37, 39].indexOf(event.keyCode) > -1) {
            $.post('/move.svg', { move: keys[event.keyCode] }, function (data) {
                body.empty();
                body.append(data);
            }).fail(function () {
                alert('error');
            });
        }
    });
});
