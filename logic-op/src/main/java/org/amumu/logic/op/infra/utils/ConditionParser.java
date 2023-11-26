package org.amumu.logic.op.infra.utils;

import com.alibaba.fastjson.JSON;
import lombok.extern.slf4j.Slf4j;
import org.amumu.logic.op.client.ExpressionEnum;
import org.amumu.logic.op.domain.model.ExpressionParam;
import org.amumu.logic.op.domain.ExpressionConditionDomain;
import org.apache.logging.log4j.util.Strings;
import org.springframework.util.ReflectionUtils;

import java.lang.reflect.Field;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

@Slf4j
public class ConditionParser {

    /**
     * 根据输入参数，执行表达式，返回结果
     * @param conditionStr 表达式
     * @param param 参数
     * @return 表达式结果
     */
    public static Boolean parser(String conditionStr, ExpressionParam param) {
        ExpressionConditionDomain condition = convert2Condition(conditionStr);
        return evaluate(condition, param);
    }

    /**
     * 执行表达式
     * @param condition 表达式
     * @param param 参数
     * @return 表达式结果
     */
    private static boolean express(ExpressionConditionDomain condition, ExpressionParam param) {

        String filed = condition.getField();
        String filedVal = retrieveFieldVal(param, filed);
        if (filedVal == null) {
            log.error("【{}】condition match retrieve 【{}】return null", condition, filed);
            return false;
        }

        String op = condition.getOp();
        List<String> valList = condition.getVal();
        return doExpress(op, valList, filedVal);
    }

    private static boolean doExpress(String op, List<String> valList, String filedVal) {
        if (ExpressionEnum.OperatorEnum.EQ.getName().equals(op)) {
            return valList.get(0).equals(filedVal);
        } else if (ExpressionEnum.OperatorEnum.NEQ.getName().equals(op)) {
            return !valList.get(0).equals(filedVal);
        } else if (ExpressionEnum.OperatorEnum.LIKE.getName().equals(op)) {
            return valList.get(0).contains(filedVal);
        } else if (ExpressionEnum.OperatorEnum.NOT_LIKE.getName().equals(op)) {
            return !valList.get(0).contains(filedVal);
        } else if (ExpressionEnum.OperatorEnum.GT.getName().equals(op)) {
            return filedVal.compareTo(valList.get(0)) > 0;
        } else if (ExpressionEnum.OperatorEnum.GTE.getName().equals(op)) {
            return filedVal.compareTo(valList.get(0)) >= 0;
        } else if (ExpressionEnum.OperatorEnum.LT.getName().equals(op)) {
            return filedVal.compareTo(valList.get(0)) < 0;
        } else if (ExpressionEnum.OperatorEnum.LET.getName().equals(op)) {
            return filedVal.compareTo(valList.get(0)) <= 0;
        } else if (ExpressionEnum.OperatorEnum.IN.getName().equals(op)) {
            return valList.contains(filedVal);
        } else if (ExpressionEnum.OperatorEnum.NOT_IN.getName().equals(op)) {
            return !valList.contains(filedVal);
        }
        throw new UnsupportedOperationException("Unsupported operation: " + op);
    }

    /**
     * 逻辑运算符，编排表达式
     * @param condition 表达式
     * @param param 参数
     * @return 逻辑运算结果
     */
    private static boolean evaluate(ExpressionConditionDomain condition, ExpressionParam param) {
        if (condition == null) {
            return false;
        }

        String type = condition.getType();
        if (ExpressionEnum.CONDITION.getName().equals(type)) {
            ExpressionConditionDomain subCondition = JSON.parseObject(condition.getConditions(), ExpressionConditionDomain.class);
            // 【条件】
            return evaluate(subCondition, param);
        } else if (ExpressionEnum.COMMON.getName().equals(type)) {
            // 【通用】
            return express(condition, param);
        } else if (ExpressionEnum.ENABLED.getName().equals(type)) {
            // 【开关】-返回开关的值【TRUE|FALSE]
            return Boolean.parseBoolean(condition.getVal().get(0));
        } else if (ExpressionEnum.LogicalOperationEnum.AND.getName().equals(type)) {
            // 【AND】
            List<ExpressionConditionDomain> subConditions = JSON.parseArray(condition.getConditions(), ExpressionConditionDomain.class);
            for (ExpressionConditionDomain subCondition : subConditions) {
                if (!evaluate(subCondition, param)) {
                    return false;
                }
            }
            return true;
        } else if (ExpressionEnum.LogicalOperationEnum.OR.getName().equals(type)) {
            // 【OR】
            List<ExpressionConditionDomain> subConditions = JSON.parseArray(condition.getConditions(), ExpressionConditionDomain.class);
            // 根据id拿到指定的条件，如果指定的条件不存在，或者返回false，再遍历剩余条件
            if (Strings.isNotBlank(param.getId())) {
                for (ExpressionConditionDomain subCondition : subConditions) {
                    if (subCondition.getId().equals(condition.getId()) && evaluate(subCondition, param)) {
                        return true;
                    }
                }
            }
            for (ExpressionConditionDomain subCondition : subConditions) {
                if (evaluate(subCondition, param)) {
                    return true;
                }
            }
            return false;
        } else if (ExpressionEnum.LogicalOperationEnum.NOT.getName().equals(type)) {
            // 【NOT】
            ExpressionConditionDomain subCondition = JSON.parseObject(condition.getConditions(), ExpressionConditionDomain.class);
            return !evaluate(subCondition, param);
        }
        return express(condition, param);
    }

    /**
     * 1、从param的extra获取val
     * 2、反射从param获取val
     * @param param 参数
     * @param fieldName 从参数获取指定field的val
     * @return field的val或者null
     */
    private static String retrieveFieldVal(ExpressionParam param, String fieldName) {
        boolean fieldExist = param.getExtra().containsKey(fieldName);
        if (fieldExist) {
            return param.getExtra().get(fieldName);
        }

        return Optional.ofNullable(ReflectionUtils.findField(param.getClass(), fieldName))
                .map(Field::toString)
                .orElse(null);
    }

    /**
     * 解析为字符串
     * @param condition 表达式条件对象
     * @return 表达式字符串
     */
    public static String convert2ConditionStr(ExpressionConditionDomain condition) {
        try {
            return JSON.toJSONString(condition);
        } catch (Exception e) {
            log.error("【{}】 condition2Str error, err:【{}】", condition, e.getMessage());
            return null;
        }
    }

    /**
     * 解析为表达式条件对象对象
     * @param conditionStr 表达式
     * @return 表达式条件对象
     */
    public static ExpressionConditionDomain convert2Condition(String conditionStr) {
        try {
            return JSON.parseObject(conditionStr, ExpressionConditionDomain.class);
        } catch (Exception e) {
            log.error("【{}】str2Condition error, err:【{}】", conditionStr, e.getMessage());
            return null;
        }
    }

    public static void main(String[] args) {
        String conditionJson = "{\"id\":\"-1\",\"name\":\"签证时效切流配置\",\"op\":null,\"val\":null,\"type\":\"condition\",\"conditions\":{\"id\":\"0\",\"name\":null,\"op\":null,\"val\":null,\"type\":\"and\",\"field\":null,\"conditions\":[{\"id\":\"1\",\"name\":\"whitelist\",\"op\":null,\"val\":[\"true\"],\"type\":\"enabled\",\"field\":null,\"conditions\":null},{\"id\":\"0\",\"name\":null,\"op\":null,\"val\":null,\"type\":\"or\",\"field\":null,\"conditions\":[{\"id\":\"1\",\"name\":\"itemId\",\"op\":\"in\",\"val\":[\"2\",\"3\"],\"type\":\"int\",\"field\":\"itemId\",\"conditions\":null},{\"id\":\"-1\",\"name\":null,\"op\":null,\"val\":null,\"type\":\"condition\",\"field\":null,\"conditions\":{\"id\":0,\"name\":null,\"op\":null,\"val\":null,\"type\":\"or\",\"field\":null,\"conditions\":[{\"name\":\"threshold\",\"id\":\"2\",\"op\":\"gte\",\"val\":[\"95\"],\"type\":\"int\",\"field\":\"threshold\",\"conditions\":null},{\"name\":\"isSeller\",\"id\":\"3\",\"op\":\"eq\",\"val\":[\"true\"],\"type\":\"boolean\",\"field\":\"isSeller\",\"conditions\":null}]}}]}]}}";

        ConditionParser parser = new ConditionParser();
        ExpressionParam param = new ExpressionParam();
        Map<String,String> map = new HashMap<>();
        map.put("threshold", "94");
        param.setExtra(map );

        try {
            boolean evaluateResult = parser.parser(conditionJson, param);
            System.out.println("evaluate result: " + evaluateResult);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
