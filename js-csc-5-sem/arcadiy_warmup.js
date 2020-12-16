'use strict';
 
/**
 * Складывает два целых числа
 * @param {Number} a Первое целое
 * @param {Number} b Второе целое
 * @throws {TypeError} Когда в аргументы переданы не числа
 * @returns {Number} Сумма аргументов
 */
function abProblem(a, b) {
    if (typeof(a) !== 'number' || typeof(b) !== 'number') {
        throw TypeError("not a number");
    }

    return a + b;
}
 
/**
 * Определяет век по году
 * @param {Number} year Год, целое положительное число
 * @throws {TypeError} Когда в качестве года передано не число
 * @throws {RangeError} Когда год – отрицательное значение
 * @returns {Number} Век, полученный из года
 */
function centuryByYearProblem(year) {
    if (typeof(year) !== 'number') {
        throw TypeError("not a number");
    }
    if (year <= 0) {
        throw RangeError("negative");
    }
    let ans = Math.floor(year / 100) + 1;
    if (ans === year / 100 + 1) {
        ans--;
    }
    return ans;
}

function letterToValue(i) {
    if (i >= '0' && i <= '9') {
        return Number.parseInt(i);
    }
    if (i < 'a' || i > 'f') {
        throw RangeError("invalid hex representation");
    }
    return i.charCodeAt(0) - 'a'.charCodeAt(0) + 10;
}

function extractColor(color, i) {
    return letterToValue(color[i]) * 16 + letterToValue(color[i + 1]);
}
 
/**
 * Переводит цвет из формата HEX в формат RGB
 * @param {String} hexColor Цвет в формате HEX, например, '#FFFFFF'
 * @throws {TypeError} Когда цвет передан не строкой
 * @throws {RangeError} Когда значения цвета выходят за пределы допустимых
 * @returns {String} Цвет в формате RGB, например, '(255, 255, 255)'
 */
function colorsProblem(hexColor) {
    if (typeof(hexColor) !== 'string') {
        throw TypeError("not a string");
    }
    hexColor = hexColor.toLowerCase();
    if (hexColor.length === 4) {
        hexColor = hexColor[0] + 
                hexColor[1] + hexColor[1] + 
                hexColor[2] + hexColor[2] + 
                hexColor[3] + hexColor[3];
    } 

    if (hexColor.length !== 7) {
        throw RangeError("invalid hex representation");
    }


    return `(${extractColor(hexColor, 1)}, ${extractColor(hexColor, 3)}, ${extractColor(hexColor, 5)})`;
}
 
/**
 * Находит n-ое число Фибоначчи
 * @param {Number} n Положение числа в ряде Фибоначчи
 * @throws {TypeError} Когда в качестве положения в ряде передано не число
 * @throws {RangeError} Когда положение в ряде не является целым положительным числом
 * @returns {Number} Число Фибоначчи, находящееся на n-ой позиции
 */
function fibonacciProblem(n) {
    if (typeof(n) !== 'number') {
        throw TypeError("not a number");
    }
    if (n < 1 || Math.floor(n) !== n) {
        throw RangeError("invalid position in a row");
    }
    if (n <= 2) {
        return 1;
    }
    let first = 1;
    let second = 1;
    let pos = 2;
    while (pos < n) {
        let next = first + second;
        first = second;
        second = next;
        pos++;
    }
    return second;
}
 
/**
 * Транспонирует матрицу
 * @param {(Any[])[]} matrix Матрица размерности MxN
 * @throws {TypeError} Когда в функцию передаётся не двумерный массив
 * @returns {(Any[])[]} Транспонированная матрица размера NxM
 */
function matrixProblem(matrix) {
    if (!Array.isArray(matrix) || matrix.length == 0 || matrix.some(elem => !Array.isArray(elem))) {
        throw TypeError("not a two-dimensional-array");
    }

    let len = matrix[0].length;

    let ans = [];
    for (let col = 0; col < len; col++) {
        ans.push([]);
        for (let row = 0; row < matrix.length; row++) {
            ans[col].push(matrix[row][col]);
        }
    }

    return ans;
}
 
/**
 * Переводит число в другую систему счисления
 * @param {Number} n Число для перевода в другую систему счисления
 * @param {Number} targetNs Система счисления, в которую нужно перевести (Число от 2 до 36)
 * @throws {TypeError} Когда переданы аргументы некорректного типа
 * @throws {RangeError} Когда система счисления выходит за пределы значений [2, 36]
 * @returns {String} Число n в системе счисления targetNs
 */
function numberSystemProblem(n, targetNs) {
    if (typeof(n) !== 'number' || typeof(targetNs) !== 'number') {
        throw TypeError("one of arguments is not a number");
    }
    if (targetNs < 2 || targetNs > 36) {
        throw RangeError("invalid targetNs");
    }
    return n.toString(targetNs);
}
 
/**
 * Проверяет соответствие телефонного номера формату
 * @param {String} phoneNumber Номер телефона в формате '8–800–xxx–xx–xx'
 * @throws {TypeError} Когда в качестве аргумента передаётся не строка
 * @returns {Boolean} Если соответствует формату, то true, а иначе false
 */
function phoneProblem(phoneNumber) {
    if (typeof(phoneNumber) !== 'string') {
        throw TypeError("not a string");
    }
    return /^8-800-[\d]{3}-[\d]{2}-[\d]{2}$/.test(phoneNumber);
}
 
/**
 * Определяет количество улыбающихся смайликов в строке
 * @param {String} text Строка в которой производится поиск
 * @throws {TypeError} Когда в качестве аргумента передаётся не строка
 * @returns {Number} Количество улыбающихся смайликов в строке
 */
function smilesProblem(text) {
    if (typeof(text) !== 'string') {
        throw TypeError("not a string");
    }
    let ans = 0;
    let copy = text;
    while (true) {
        let pos = copy.search(/:-\)/);
        if (pos != -1) {
            ans++;
            copy = copy.slice(pos + 3);
        } else {
            break;
        }
    }

    copy = text;
    while (true) {
        let pos = copy.search(/\(-:/);
        if (pos != -1) {
            ans++;
            copy = copy.slice(pos + 3);
        } else {
            break;
        }
    }
    return ans;
}

function checkRow(sx, sy, i, j, ch, field) {
    let x = sx;
    let y = sy;
    for (let step = 0; step < 3; step++) {
        if (field[x][y] !== ch) {
            return false;
        }
        x += i;
        y += j;
    }
    
    return true;
}

function checkForWinner(field, ch) {
    return (
        checkRow(0, 0, 0, 1, ch, field) || 
        checkRow(1, 0, 0, 1, ch, field) || 
        checkRow(2, 0, 0, 1, ch, field) || 

        checkRow(0, 0, 1, 0, ch, field) || 
        checkRow(0, 1, 1, 0, ch, field) ||
        checkRow(0, 2, 1, 0, ch, field) || 

        checkRow(0, 0, 1, 1, ch, field) ||
        checkRow(2, 0, -1, 1, ch, field)
    );
}

/**
 * Определяет победителя в игре "Крестики-нолики"
 * Тестами гарантируются корректные аргументы.
 * @param {(('x' | 'o')[])[]} field Игровое поле 3x3 завершённой игры
 * @returns {'x' | 'o' | 'draw'} Результат игры
 */
function ticTacToeProblem(field) {
    if (checkForWinner(field, 'x')) {
        return 'x';
    }
    if (checkForWinner(field, 'o')) {
        return 'o';
    }
    return 'draw';
}
 
module.exports = {
    abProblem,
    centuryByYearProblem,
    colorsProblem,
    fibonacciProblem,
    matrixProblem,
    numberSystemProblem,
    phoneProblem,
    smilesProblem,
    ticTacToeProblem
};
