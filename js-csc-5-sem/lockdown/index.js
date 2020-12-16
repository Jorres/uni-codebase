const smartSend = (f, timeoutMs) => {
    let timer = null;
    return function () {
        clearTimeout(timer);
        timer = setTimeout(f, timeoutMs);
    }
};

const sselect = (selector) => {
    return document.querySelector(selector);
};

const askApi = (prefix) => {
    let url = `https://autocomplete.travelpayouts.com/places2?term=${prefix}`;

    return new Promise((resolve, reject) => {
        let xhttp = new XMLHttpRequest();
        xhttp.onreadystatechange = function() {
            if (this.readyState == 4) { // FULLY DONE
                if (this.status == 200) {
                    resolve(xhttp.responseText);
                } else {
                    reject();
                }
            }
        };
        xhttp.open("GET", url, /* async = */ true);
        xhttp.send();
    });

    // Грустный комментарий - фетч пробовал самыми разными способами, и через проксю cors-anywhere, 
    // и напрямую. максимум чего я достиг - это 45 секунд на запрос и 200ок. Xhttp отрабатывает 20ms.
    // Я разбирался 10 часов и не разобрался. Чтобы соответствовать заданию, обернул в промис сам. 
    //
    // let proxy = "https://cors-anywhere.herokuapp.com/";
    // fetch(proxy + real).then(response => {
    //     return response.json();
    // }).then(r => {
    //     console.log(r);
    // });
};

const substituteSuggestion = (text) => {
    let floating = sselect('.search-bar__floating');
    let inputField = sselect(".search-bar__airport-input");

    inputField.value = text;
    floating.style.display = "none";
};

const updateSuggestions = (prefix) => {
    let floating = sselect('.search-bar__floating');
    if (prefix.length == 0) {
        floating.style.display = "none";    
        return;
    }

    askApi(prefix)
        .then(data => {
            data = JSON.parse(data);
            floating.style.display = "flex";
            floating.innerHTML = "";
            for (let city of data) {
                let p = document.createElement("input");
                let suggestionText = `${city.name}, ${city.type}, ${city.code}`;

                p.type = "text";
                p.readOnly = true;
                p.value = suggestionText;
                p.classList.add("search-bar__suggestion");
                p.classList.add("tabable");
                floating.appendChild(p);
                p.selectionStart = p.selectionEnd;

                p.addEventListener("click", () => substituteSuggestion(suggestionText));
                p.addEventListener("keypress", (event) => {
                    if (event.which === 13) {
                        substituteSuggestion(suggestionText);
                    }
                });
            }
        });

};

const init = () => {
    const inputName = sselect(".search-bar__airport-input");
    inputName.placeholder = "Начните печатать название или код аэропорта...";
    inputName.addEventListener("input", smartSend(() => updateSuggestions(inputName.value), 500));

    document.addEventListener("keydown", (event) => {
        if (sselect(".search-bar__floating").style.display == "none") {
            return;
        }
        let k = event.which;
        if (k == 38 || k == 40) {
            event.preventDefault();
            let sugs = document.querySelectorAll(".tabable");
            for (let i = 0; i < sugs.length; i++) {
                if (sugs[i] == document.activeElement) {
                    let req;
                    if (k == 40) {
                        req = (i + 1) % sugs.length;
                    } else {
                        req = (i + sugs.length - 1) % sugs.length;
                    }
                    sugs[req].focus();
                    break;
                }
            };
        }
    })
};

document.addEventListener("DOMContentLoaded", init);
