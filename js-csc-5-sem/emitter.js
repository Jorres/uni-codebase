'use strict';
// let students = {
//   Sam: {
//     focus: 100,
//     wisdom: 50
//   },
//   Sally: {
//     focus: 100,
//     wisdom: 60
//   },
//   Bill: {
//     focus: 90,
//     wisdom: 50
//   },
//   Sharon: {
//     focus: 110,
//     wisdom: 40
//   }
// };

function getEmitter() {
    let newNode = function(name) {
        return {
            name: name,
            nexts: {},
            events: []
        };
    };

    let root = newNode("");

    return {
        on: function (event, context, handler) {
            let cur = root;
            event.split('.').forEach(step => {
                if (!cur.nexts.hasOwnProperty(step)) {
                    let node = newNode(step);
                    cur.nexts[step] = node;
                }
                cur = cur.nexts[step];
            });
            cur.events.push({context, handler});
            return this;
        },

        clearup: function (node, targetContext) {
            node.events = node.events.filter(
                ({context, handler}) => context != targetContext
            );
            for (let key in node.nexts) {
                this.clearup(node.nexts[key], targetContext);
            }
        },

        off: function (event, context) {
            let cur = root;
            let shouldClear = true;
            for (let step of event.split('.')) {
                if (!cur.nexts.hasOwnProperty(step)) {
                    shouldClear = false;
                    break;
                }
                cur = cur.nexts[step];
            }
            if (shouldClear) {
                this.clearup(cur, context);
            }
            return this;
        },
 
        emit: function (event) {
            let cur = root;
            let path = [cur];
            for (let step of event.split('.')) {
                if (!cur.nexts.hasOwnProperty(step)) {
                    break;
                } else {
                    cur = cur.nexts[step];
                    path.push(cur);
                }
            }

            path.reverse().forEach(node => {
                node.events.forEach(({context, handler}) => {
                    handler.call(context);
                })
            });

            return this;
        },

        several: function (event, context, handler, times) {
            console.info(event, context, handler, times);
        },

        through: function (event, context, handler, frequency) {
            console.info(event, context, handler, frequency);
        }

    };
}



// let lecturer = getEmitter();
//
// // С началом лекции у всех резко повышаются показатели
// lecturer
//   .on('begin', students.Sam, function () {
//     this.focus += 10;
//   })
//   .on('begin', students.Sally, function () {
//     this.focus += 10;
//   })
//   .on('begin', students.Bill, function () {
//     this.focus += 10;
//     this.wisdom += 5;
//   })
//   .on('begin', students.Sharon, function () {
//     this.focus += 20;
//   });
//
// // На каждый слайд внимательность падает, но растет мудрость
// lecturer
//   .on('slide', students.Sam, function () {
//     this.wisdom += Math.round(this.focus * 0.1);
//     this.focus -= 10;
//   })
//   .on('slide', students.Sally, function () {
//     this.wisdom += Math.round(this.focus * 0.15);
//     this.focus -= 5;
//   })
//   .on('slide', students.Bill, function () {
//     this.wisdom += Math.round(this.focus * 0.05);
//     this.focus -= 10;
//   })
//   .on('slide', students.Sharon, function () {
//     this.wisdom += Math.round(this.focus * 0.01);
//     this.focus -= 5;
//   });
//
// // На каждый веселый слайд всё наоборот
// lecturer
//   .on('slide.funny', students.Sam, function () {
//     this.focus += 5;
//     this.wisdom -= 10;
//   })
//   .on('slide.funny', students.Sally, function () {
//     this.focus += 5;
//     this.wisdom -= 5;
//   })
//   .on('slide.funny', students.Bill, function () {
//     this.focus += 5;
//     this.wisdom -= 10;
//   })
//   .on('slide.funny', students.Sharon, function () {
//     this.focus += 10;
//     this.wisdom -= 10;
//   });
//
// // Начинаем лекцию
// lecturer.emit('begin');
// // Sam(110,50); Sally(110,60); Bill(100,55); Sharon(130,40)
// console.log(students);
//
// lecturer
//   .emit('slide.text')
//   .emit('slide.text')
//   .emit('slide.text')
//   .emit('slide.funny');
// // Sam(75,79); Sally(95,118); Bill(65,63); Sharon(120,34)
// console.log(students);
//
// lecturer
//   .off('slide.funny', students.Sharon)
//   .emit('slide.text')
//   .emit('slide.text')
//   .emit('slide.funny');
// // Sam(50,90); Sally(85,155); Bill(40,62); Sharon(105,37)
// console.log(students);
//
// lecturer
//   .off('slide', students.Bill)
//   .emit('slide.text')
//   .emit('slide.text')
//   .emit('slide.text');
//
// lecturer.emit('end');
// // Sam(20,102); Sally(70,191); Bill(40,62); Sharon(90,40)
// console.log(students);

let isExtraTaskSolved = false;
 
module.exports = {
    getEmitter, isExtraTaskSolved
};

