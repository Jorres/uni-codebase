'use strict';
 
const phoneBook = new Map();
 
function syntaxError(lineNumber, charNumber) {
    lineNumber += 1;
    charNumber += 1;
    throw new Error(`SyntaxError: Unexpected token at ${lineNumber}:${charNumber}`);
}

const allowedAfterCreate = {
    "Создай": ["контакт"],
    "контакт": ["name"]
};

const allowedAfterDelete = {
    "Удали": ["контакты,", "контакт", "телефон", "почту"],
    "контакт": ["name"],
    "телефон": ["phone"],
    "почту": ["mail"],
    "и": ["телефон", "почту"],
    "phone": ["и", "для"],
    "mail": ["и", "для"],
    "для": ["контакта"],
    "контакта": ["name"],
    "контакты,": ["где"],
    "где": ["есть"],
    "есть": ["request"]
};

const allowedAfterAdd = {
    "Добавь": ["телефон", "почту"],
    "телефон": ["phone"],
    "почту": ["mail"],
    "и": ["телефон", "почту"],
    "phone": ["и", "для"],
    "mail": ["и", "для"],
    "для": ["контакта"],
    "контакта": ["name"]
}

const allowedAfterShow = {
    "Покажи": ["имя", "почты", "телефоны"],
    "имя": ["для", "и"],
    "почты": ["для", "и"],
    "телефоны": ["для", "и"],
    "и": ["имя", "почты", "телефоны"],
    "для": ["контактов,"],
    "контактов,": ["где"],
    "где": ["есть"],
    "есть": ["request"]
};

const allowedAfter = {
    "Покажи": allowedAfterShow,
    "Создай": allowedAfterCreate,
    "Удали": allowedAfterDelete,
    "Добавь": allowedAfterAdd
};

function parse(lineNumber, query, pos, expectedWords) {
    // returns pos following end of the word
    for (let expected of expectedWords) {
        let lastChar = query[query.length - 1];
        if (expected == 'name' || expected == 'request') {
            if (lastChar != ';') {
                syntaxError(lineNumber, query.length);
            }
            return [query.length - 1, expected];
        } else if (expected == 'mail') {
            for (let i = pos; i < query.length; i++) {
                if (query[i] == ' ' || query[i] == ';') {
                    return [i, expected];
                }
            }
        } else if (expected == 'phone') {
            let phoneEndPos = pos + 10;
            for (let i = pos; i < phoneEndPos; i++) {
                if (i == query.length || ('0' > query[i] || query[i] > '9')) {
                    return [pos, expected];
                }
            }
            return [(query[phoneEndPos] == ' ' ? phoneEndPos : pos), expected];
        }

        let bound = pos + expected.length;
        let found = true;
        for (let i = pos; i < bound; i++) {
            if (query[i] != expected[i - pos]) {
                found = false;
                break;
            }
        }
        if (found) {
            return [bound, expected];
        }
    }
    return [pos, expectedWords[0]];
}

function skipSpaces(query, pos) {
    console.assert(pos < query.length && query[pos] == ' ', 'not a whitespace in skipSpaces');
    if (pos + 1 < query.length && query[pos + 1] == ' ') {
        return pos;
    }
    return pos + 1;
}

function parseQuery(query, lineNumber) {
    let identifier = query[0];
    let mapToWord = {
        'П': "Покажи",
        'У': "Удали",
        'Д': "Добавь",
        'С': "Создай"
    };
    let expected = mapToWord[identifier];
    if (!expected) {
        syntaxError(lineNumber, 0);
    }
    let curLang = allowedAfter[expected];
    let pos = 0;
    let tokens = [];

    expected = [expected]; // crutch 
    while (pos < query.length) {
        let parseResult = parse(lineNumber, query, pos, expected);
        let newpos = parseResult[0];
        let parsedType = parseResult[1];

        if (pos == newpos && (expected != 'name' && expected != 'request')) {
            syntaxError(lineNumber, pos);
        }

        let parsed = query.substring(pos, newpos);
        pos = newpos;
        tokens.push(parsed);
        expected = curLang[parsedType];
        if (query[pos] != ';') {
          if (expected == 'name' || expected == 'request') {
            newpos = pos + 1;
          } else {
            newpos = skipSpaces(query, pos);
            if (pos == newpos) {
                syntaxError(lineNumber, pos + 1);
            }
          }
          pos = newpos;
        } else {
            break;
        }
    }


    return tokens;                   
}

function createBlankUser(name) {
    return {
        'name': name,
        'emails': new Set(),
        'phones': new Set()
    };
}

function processCreate(tokens) {
    let name = tokens[tokens.length - 1];
    if (!phoneBook.has(name)) {
        phoneBook.set(name, createBlankUser(name));
    }
}

function filterPeople(request) {
    let result = [];
    phoneBook.forEach((person) => {
        let flag = false;
        if (person.name.includes(request)) {
            flag = true;
        } 
        for (let phone of person.phones) {
            if (phone.includes(request)) {
                flag = true;
                break;
            }
        }
        for (let email of person.emails) {
            if (email.includes(request)) {
                flag = true;
                break;
            }
        }
        if (flag) {
            result.push(person);
        }
    });
    return result;
}

function collectPhonesAndEmails(tokens) {
    let phones = [];
    let emails = [];
    for (let i = 1;;) {
        if (tokens[i] == "телефон") {
            phones.push(tokens[i + 1]);
        } else if (tokens[i] == "почту") {
            emails.push(tokens[i + 1]);
        }
        i += 2;
        if (tokens[i] != "и") {
            break;
        }
        i += 1;
    }
    return [phones, emails];
}


function processDelete(tokens) {
    if (tokens[1] == 'контакт') {
        let name = tokens[tokens.length - 1];
        // if (phoneBook.has(name)) {
            phoneBook.delete(name);
        // }
    } else if (tokens[1] == 'контакты,') {
        let request = tokens[4];
        if (request == '') {
            return;
        }
        let filteredData = filterPeople(request); 
        for (let person of filteredData) {
            phoneBook.delete(person.name);
        }
    } else {
        let name = tokens[tokens.length - 1];
        if (phoneBook.has(name)) {
            let [phones, emails] = collectPhonesAndEmails(tokens);
            let personPhones = phoneBook.get(name).phones;
            phones.forEach((phone) => {
                personPhones.delete(phone);
            });

            let personEmails = phoneBook.get(name).emails;
            emails.forEach((email) => {
                personEmails.delete(email);
            });
        }
    }
}

function processAdd(tokens) {
    let name = tokens[tokens.length - 1];
    if (phoneBook.has(name)) {
        let [phones, emails] = collectPhonesAndEmails(tokens);
        let person = phoneBook.get(name);
        phones.forEach(phone => {
            person.phones.add(phone);
        });

        emails.forEach(email => {
            person.emails.add(email);
        });
    }
}

function formatPhone(phone) {
    let a = phone.substring(0, 3);
    let b = phone.substring(3, 6);
    let c = phone.substring(6, 8);
    let d = phone.substring(8, 10);
    return `+7 (${a}) ${b}-${c}-${d}`;
}

function processShow(tokens) {
    let request = tokens[tokens.length - 1];
    if (request == '') {
        return;
    }

    let order = [];
    for (let i = 1;;) {
        if (tokens[i] == "телефоны") {
            order.push('p');
        } else if (tokens[i] == "почты") {
            order.push('e');
        } else if (tokens[i] == "имя") {
            order.push('n');
        }

        if (tokens[i + 1] == "для") {
            break;
        }
        i += 2;
    }

    let result = [];
    let filteredData = filterPeople(request);

    for (let person of filteredData) {
        let names = '';
        let phones = '';
        let emails = '';
        names += person.name;

        person.emails.forEach(email => emails += email + ',');
        emails = emails.substring(0, emails.length - 1);

        person.phones.forEach(phone => phones += formatPhone(phone) + ',');
        phones = phones.substring(0, phones.length - 1);

        let cur = '';
        for (let i = 0; i < order.length; i++) {
            let which = order[i];
            if (which == 'e') {
                cur += emails;
            } else if (which == 'p') {
                cur += phones; 
            } else {
                cur += names;
            }
            if (i != order.length - 1) {
                cur += ';';
            }
        }
        result.push(cur);
    }
    return result;
}

function processQuery(tokens) {
    const processors = {
        "Создай": processCreate,
        "Удали": processDelete,
        "Добавь": processAdd,
        "Покажи": processShow
    };
    return processors[tokens[0]](tokens);
}
 
function run(query) {
    let queries = query.split(';');
    for (let i = 0; i < queries.length - 1; i++) {
        queries[i] += ';';
    }
    if (queries[queries.length - 1] == '') {
        queries.pop();
    }

    let result = [];
    queries.forEach((query, id) => {
        let localResult = processQuery(parseQuery(query, id));
        if (localResult) {
            localResult.forEach(str => result.push(str));
        }
    });
    return result;
} 
 
module.exports = { phoneBook, run };

function tests() {
  let query =
      'Создай контакт Григорий   B;' +
      'Покажи имя для контактов, где есть ий;';
    console.log(run(query));
}

// tests();
