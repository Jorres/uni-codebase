const proto = {
    someMethod = () => {};
};

function fabric () {
    return {a: 'a'}; 

    // tmp.setPrototypeOf(proto);
}

Object.create();

function Obj() {
    this.prop = "setup"; 
    // this ссылается на создаваемый объект
}

let obj = new Obj();

// Можно устроить наследование на прототипах
//
function B(args) {
    // super(args);
    // cупер ссылается на прототип объекта, который указан в [[HomeObject]]
    // super == Object.getPrototypeOf(student.getName.[[HomeObject]]);
    // A.call(this, args)
    // через супер нельзя перезатереть, его нельзя удалить, можно вызывать статические методы

    this.prop = "setup"; 
}

// Фабрики приятнее и очевиднее, this работает как положено
//

// В классах есть конструктор, статические методы и обычные методы
// есть публичные и приватные поля, приватные с решеточки и нифига не поддерживаются.
// даже публичные не везде
// а лучше в конструкторе 
//

