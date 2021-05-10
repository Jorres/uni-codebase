const action = () => {
    let p = new Promise((resolve, reject) => {
        setTimeout(() => reject(), 1000);
        setTimeout(() => resolve(3), 2000);
    });

    p.catch((reason) => {
        console.log(reason + " once"); 
        return 4;
    }).then((value) => {
        console.log("promise completed with " + value)
    }).catch((reason) => {
        console.log(reason + " never"); 
    });
}

action();
