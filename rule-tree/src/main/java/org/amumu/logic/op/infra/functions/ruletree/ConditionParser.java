package org.amumu.logic.op.infra.functions.ruletree;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.amumu.logic.op.client.RuleTreeEnum;
import org.amumu.logic.op.domain.RuleTreeConditionDomain;
import org.amumu.logic.op.domain.model.RuleTreeParam;
import org.amumu.logic.op.infra.utils.JsonUtil;
import org.springframework.util.ReflectionUtils;

import java.lang.reflect.Field;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

@Slf4j
public class ConditionParser {

    private static PathChainFactory PATH_CHAIN_FACTORY;

    public static void setPathChainFactory(PathChainFactory pathChainFactory) {
        PATH_CHAIN_FACTORY = pathChainFactory;
    }

    /**
     * 根据输入参数，执行表达式，返回结果
     * @param conditionStr 表达式
     * @param param 参数
     * @return 表达式结果
     */
    public static Boolean parser(String conditionStr, RuleTreeParam param) {
        RuleTreeConditionDomain condition = convert2Condition(conditionStr);
        String id = Optional.ofNullable(condition).map(RuleTreeConditionDomain::getId).orElse(null);
        String name = Optional.ofNullable(condition).map(RuleTreeConditionDomain::getName).orElse(null);

        PATH_CHAIN_FACTORY.initPath(id, name);
        return evaluate(condition, param);
    }

    /**
     * 逻辑运算符，编排表达式
     * @param condition 表达式
     * @param param 参数
     * @return 逻辑运算结果
     */
    public static boolean evaluate(RuleTreeConditionDomain condition, RuleTreeParam param) {
        if (condition == null) {
            return false;
        }

        String type = condition.getType();
        if (RuleTreeEnum.CONDITION.getName().equals(type)) {
            RuleTreeConditionDomain subCondition = JsonUtil.jsonNode2obj(condition.getConditions(), RuleTreeConditionDomain.class);
            // 【条件】
            return evaluate(subCondition, param);
        } else if (RuleTreeEnum.ENABLED.getName().equals(type)) {
            // 【开关】-返回开关的值【TRUE|FALSE]
            boolean result = Boolean.parseBoolean(condition.getVal().get(0));
            PATH_CHAIN_FACTORY.chainPath(null, condition, result);
            return result;
        } else if (RuleTreeEnum.LogicalOperationEnum.AND.getName().equals(type)) {
            return LogicOperator.AND.operator(condition.getConditions(), param);
        } else if (RuleTreeEnum.LogicalOperationEnum.OR.getName().equals(type)) {
            return LogicOperator.OR.operator(condition.getConditions(), param);
        } else if (RuleTreeEnum.LogicalOperationEnum.NOT.getName().equals(type)) {
            return LogicOperator.NOT.operator(condition.getConditions(), param);
        }
        return express(condition, param);
    }

    /**
     * 执行表达式
     * @param condition 表达式
     * @param param 参数
     * @return 表达式结果
     */
    private static boolean express(RuleTreeConditionDomain condition, RuleTreeParam param) {

        String filed = condition.getField();
        String filedVal = retrieveFieldVal(param, filed);
        if (filedVal == null) {
            log.error("【{}】condition match retrieve【{}】return null", condition, filed);
            PATH_CHAIN_FACTORY.chainPath(null, condition, false);
            return false;
        }

        String op = condition.getOp();
        List<String> valList = condition.getVal();
        boolean result = doExpress(op, valList, filedVal);
        PATH_CHAIN_FACTORY.chainPath(filedVal, condition, result);
        return result;
    }

    private static boolean doExpress(String op, List<String> valList, String filedVal) {
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
     * 1、从param的extra获取val
     * 2、反射从param获取val
     * @param param 参数
     * @param fieldName 从参数获取指定field的val
     * @return field的val或者null
     */
    private static String retrieveFieldVal(RuleTreeParam param, String fieldName) {
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
    public static String convert2conditionStr(RuleTreeConditionDomain condition) {
        try {
            return JsonUtil.obj2JsonStr(condition);
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
    public static RuleTreeConditionDomain convert2Condition(String conditionStr) {
        try {
            return JsonUtil.str2obj(conditionStr, RuleTreeConditionDomain.class);
        } catch (Exception e) {
            log.error("【{}】str2Condition error, err:【{}】", conditionStr, e.getMessage());
            return null;
        }
    }

    public static void main(String[] args) {
        PATH_CHAIN_FACTORY = new PathChainFactory();
        JsonUtil.setPathChainFactory(new ObjectMapper());
        Map<String,String> map = new HashMap<>();
        map.put("threshold", "94");
        RuleTreeParam param = new RuleTreeParam();
        param.setExtra(map);

        String conditionJson = "{\"id\":\"-1\",\"name\":\"签证时效切流配置\",\"op\":null,\"val\":null,\"type\":\"condition\",\"conditions\":{\"id\":\"0\",\"name\":null,\"op\":null,\"val\":null,\"type\":\"and\",\"field\":null,\"conditions\":[{\"id\":\"1\",\"name\":\"xxxSwitch\",\"op\":null,\"val\":[\"true\"],\"type\":\"enabled\",\"field\":null,\"conditions\":null},{\"id\":\"2\",\"name\":null,\"op\":null,\"val\":null,\"type\":\"or\",\"field\":null,\"conditions\":[{\"id\":\"3\",\"name\":\"itemId\",\"op\":\"in\",\"val\":[\"2\",\"3\"],\"type\":\"int\",\"field\":\"itemId\",\"conditions\":null},{\"id\":\"4\",\"name\":null,\"op\":null,\"val\":null,\"type\":\"condition\",\"field\":null,\"conditions\":{\"id\":0,\"name\":null,\"op\":null,\"val\":null,\"type\":\"or\",\"field\":null,\"conditions\":[{\"name\":\"threshold\",\"id\":\"5\",\"op\":\"gte\",\"val\":[\"95\"],\"type\":\"int\",\"field\":\"threshold\",\"conditions\":null},{\"name\":\"isSeller\",\"id\":\"6\",\"op\":\"eq\",\"val\":[\"true\"],\"type\":\"boolean\",\"field\":\"isSeller\",\"conditions\":null}]}}]}]}}";
        boolean result = ConditionParser.parser(conditionJson, param);
        System.out.println("evaluate result: " + result);
        System.out.println("evaluate path: ");
        PATH_CHAIN_FACTORY.printPath();
    }
}
