let pics = ['pig-a', 'pig-b', 'pig-c', 'pig-d', 'pig-e', 'pig-f', 'last'];
let lastActive = 0;

const topCurPicture = () => {
    return document.querySelector('.' + pics[lastActive]);
}

const popCurPicture = () => {
    const pic = topCurPicture();
    lastActive++;
    return pic;
}

const trigger = (direction, animation) => {
    const pic = popCurPicture();
    if (pic && lastActive < pics.length) {
        if (direction == 'right') {
            pic.style.transform = 'translate(220%) rotate(20deg)';
        } else {
            pic.style.transform = 'translate(-220%) rotate(-20deg)';
        }

        document.querySelector('body').style.animation = animation + ' 0.5s';
        setTimeout(() => {
            pic.style.display = 'none';
            document.querySelector('body').style.animation = 'none';
        }, 500);
    }
    const pic2 = topCurPicture();
    if (pic2) {
        pic2.style.display = 'flex';
        if (lastActive == pics.length - 1) {
            pic2.style.opacity = '1';
        }
    }
}

const init = () => {
    let data = [ ['dislike', trigger, 'left', 'pulseRed'], 
                 ['superlike', trigger, 'right', 'pulseYellow'], 
                 ['like', trigger, 'right', 'pulseGreen'] ];

    data.forEach(([btn, action, direction, animation]) => {
        const elem = document.querySelector('.' + btn);
        elem.addEventListener('click', () => action(direction, animation));
    });
}

document.addEventListener("DOMContentLoaded", init);
