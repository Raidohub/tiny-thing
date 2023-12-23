package org.amumu.rule.tree.infra.functions.ruletree;

import org.amumu.rule.tree.client.RuleTreeEnum;
import org.amumu.rule.tree.domain.RuleTreeConditionDomain;
import java.util.List;

/**
 * 运算符
 */
public class Operator {

    /**
     * 通用运算表达式
     * @param op 通用运算符
     * @param valList 条件值列表
     * @param filedVal 参数字段值
     * @return boolean
     */
    public static boolean commonOperate(String op, List<String> valList, String filedVal) {
        String val = valList.get(0);
        if (RuleTreeEnum.OperatorEnum.EQ.getName().equals(op)) {
            return val.equals(filedVal);
        } else if (RuleTreeEnum.OperatorEnum.NEQ.getName().equals(op)) {
            return !val.equals(filedVal);
        } else if (RuleTreeEnum.OperatorEnum.LIKE.getName().equals(op)) {
            return val.contains(filedVal);
        } else if (RuleTreeEnum.OperatorEnum.NOT_LIKE.getName().equals(op)) {
            return !val.contains(filedVal);
        } else if (RuleTreeEnum.OperatorEnum.GT.getName().equals(op)) {
            return filedVal.compareTo(val) > 0;
        } else if (RuleTreeEnum.OperatorEnum.GTE.getName().equals(op)) {
            return filedVal.compareTo(val) >= 0;
        } else if (RuleTreeEnum.OperatorEnum.LT.getName().equals(op)) {
            return filedVal.compareTo(val) < 0;
        } else if (RuleTreeEnum.OperatorEnum.LET.getName().equals(op)) {
            return filedVal.compareTo(val) <= 0;
        } else if (RuleTreeEnum.OperatorEnum.IN.getName().equals(op)) {
            return valList.contains(filedVal);
        } else if (RuleTreeEnum.OperatorEnum.NOT_IN.getName().equals(op)) {
            return !valList.contains(filedVal);
        }
        throw new UnsupportedOperationException("Unsupported operation: " + op);
    }

    /**
     * 开关表达式
     * @param condition 表达式
     * @return boolean
     */
    public static boolean enableOperate(RuleTreeConditionDomain condition) {
        return Boolean.parseBoolean(condition.getVal().get(0));
    }
}
