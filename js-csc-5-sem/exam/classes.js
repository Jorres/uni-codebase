class A { // тупо еще одна функция
    _fieldName = "";
    
    constructor() {

        super()
        super.propName

        this.fieldName = "";        
    }

    get fieldName() {
        return this._fieldName;    
    }

    set fieldName(value) {
        this._fieldName = value; 
    }
}

let a = new A();
a.fieldName = "newName";
console.log(a.fieldName);

// строже, чем конструкторы. В классе строгий режим, нельзя вызвать без new
//
// классы тоже функции, поэтому их тоже можно генерировать и возвращать return class {}
