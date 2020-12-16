let stId = 0;
let anecdotes = ['Я анекдот!', "Посмейтесь, пожалуйста...", "Нас уже целых три штуки!", "А может, и четыре!"]

const showNew = () => {
    const holder = document.querySelector('.widget-area__anecdote-holder');
    stId = (stId + 1) % anecdotes.length;
    holder.innerHTML = anecdotes[stId];
}

document.addEventListener("DOMContentLoaded", () => {
    document.querySelectorAll('.settings__label').forEach(label => {
        label.addEventListener('click', () => {
            const check = label.querySelector('input');
            const elem = document.querySelector('.' + check.className.substr("settings__".length));
            if (check.checked) {
                elem.style.display = "block";
            } else {
                elem.style.display = "none";
            }
        })    
    });

    document.querySelector(".widget-area__show-more").addEventListener('click', showNew);
});
