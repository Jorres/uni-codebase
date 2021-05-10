/**
 * В теле класса решения разрешено использовать только переменные делегированные в класс RegularInt.
 * Нельзя volatile, нельзя другие типы, нельзя блокировки, нельзя лазить в глобальные переменные.
 *
 * @author Tarasov Egor
 */
class Solution : MonotonicClock {
    private var r11 by RegularInt(0)
    private var r12 by RegularInt(0)
    private var r13 by RegularInt(0)
    private var r21 by RegularInt(0)
    private var r22 by RegularInt(0)
    private var r23 by RegularInt(0)

    override fun write(time: Time) {
        r21 = time.d1
        r22 = time.d2
        r23 = time.d3
        r13 = time.d3
        r12 = time.d2
        r11 = time.d1
    }

    override fun read(): Time {
        val t11 = r11
        val t12 = r12
        val t13 = r13
        val t23 = r23
        val t22 = r22
        val t21 = r21
        if (t11 < t21) {
            return Time(t21, 0, 0)
        } else if (t12 < t22) {
            return Time(t11, t22, 0)
        } else {
            return Time(t11, t12, t23)
        }
    }
}
