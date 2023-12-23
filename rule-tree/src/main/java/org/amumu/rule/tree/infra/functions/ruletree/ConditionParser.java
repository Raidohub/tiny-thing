package org.amumu.rule.tree.infra.functions.ruletree;

import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.amumu.rule.tree.client.RuleTreeEnum;
import org.amumu.rule.tree.domain.RuleTreeConditionDomain;
import org.amumu.rule.tree.domain.model.RuleTreeParam;
import org.amumu.rule.tree.infra.config.json.JacksonConfig;
import org.amumu.rule.tree.infra.utils.JsonUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.ReflectionUtils;
import java.lang.reflect.Field;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

@Slf4j
@Component
public class ConditionParser {

    @Getter
    @Autowired
    private static NodeChainFactory NODE_CHAIN_FACTORY;

    /**
     * 根据输入参数，执行表达式，返回结果
     * @param conditionStr 表达式
     * @param param 参数
     * @return 表达式结果
     */
    public static Boolean parser(String conditionStr, RuleTreeParam param) {
        RuleTreeConditionDomain condition = JsonUtil.str2obj(conditionStr, RuleTreeConditionDomain.class);
        NODE_CHAIN_FACTORY.initPath(condition.getId(), condition.getName());
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
            return enableOperate(condition);
        } else if (RuleTreeEnum.LogicalOperationEnum.enumValueOf(type) != null) {
            return logicOperate(condition, param, type);
        }
        return commonOperate(condition, param);
    }

    /**
     * 逻辑运算
     * @param condition 表达式
     * @param param 参数
     * @param type AND OR NOT
     * @return boolean
     */
    private static boolean logicOperate(RuleTreeConditionDomain condition, RuleTreeParam param, String type) {
        NODE_CHAIN_FACTORY.setupChain(null, condition, null);
        if (RuleTreeEnum.LogicalOperationEnum.AND.getName().equals(type)) {
            return LogicOperator.AND.operate(condition.getConditions(), param);
        } else if (RuleTreeEnum.LogicalOperationEnum.OR.getName().equals(type)) {
            return LogicOperator.OR.operate(condition.getConditions(), param);
        }
        return LogicOperator.NOT.operate(condition.getConditions(), param);
    }

    /**
     * 执行开关表达式
     * @param condition 表达式
     * @return boolean
     */
    private static boolean enableOperate(RuleTreeConditionDomain condition) {
        boolean result = Operator.enableOperate(condition);
        NODE_CHAIN_FACTORY.setupChain(null, condition, result);
        return result;
    }

    /**
     * 执行表达式
     * @param condition 表达式
     * @param param 参数
     * @return boolean
     */
    private static boolean commonOperate(RuleTreeConditionDomain condition, RuleTreeParam param) {
        String filed = condition.getField();
        String filedVal = retrieveFieldVal(param, filed);
        if (filedVal == null) {
            log.error("【{}】condition match retrieve【{}】return null", condition, filed);
            NODE_CHAIN_FACTORY.setupChain(null, condition, false);
            return false;
        }

        String op = condition.getOp();
        List<String> valList = condition.getVal();
        boolean result = Operator.commonOperate(op, valList, filedVal);
        NODE_CHAIN_FACTORY.setupChain(filedVal, condition, result);
        return result;
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

    public static void main(String[] args) {
        NODE_CHAIN_FACTORY = new NodeChainFactory();
        JsonUtil.setOBJECT_MAPPER(new JacksonConfig().objectMapper());
        Map<String,String> map = new HashMap<>();
        map.put("threshold", "94");
        map.put("itemId", "200");
        RuleTreeParam param = new RuleTreeParam();
        param.setExtra(map);

        String conditionJson = "{\"id\":\"-1\",\"name\":\"签证时效切流配置\",\"op\":null,\"val\":null,\"type\":\"condition\",\"conditions\":{\"id\":\"0\",\"pid\":\"-1\",\"name\":null,\"op\":null,\"val\":null,\"type\":\"and\",\"field\":null,\"conditions\":[{\"id\":\"1\",\"pid\":\"0\",\"name\":null,\"op\":null,\"val\":null,\"type\":\"or\",\"field\":null,\"conditions\":[{\"id\":\"10\",\"pid\":\"1\",\"name\":\"Switch\",\"op\":null,\"val\":[\"false\"],\"type\":\"enabled\",\"field\":null,\"conditions\":null},{\"id\":\"11\",\"pid\":\"1\",\"name\":null,\"op\":null,\"val\":null,\"type\":\"or\",\"field\":null,\"conditions\":[{\"id\":\"110\",\"pid\":\"11\",\"name\":\"buyerId\",\"op\":null,\"val\":[\"20\"],\"type\":\"enabled\",\"field\":null,\"conditions\":null},{\"id\":\"111\",\"pid\":\"11\",\"name\":\"Switch\",\"op\":null,\"val\":[\"true\"],\"type\":\"enabled\",\"field\":null,\"conditions\":null}]}]},{\"id\":\"2\",\"pid\":\"0\",\"name\":null,\"op\":null,\"val\":null,\"type\":\"or\",\"field\":null,\"conditions\":[{\"id\":\"3\",\"pid\":\"2\",\"name\":\"itemId\",\"op\":\"in\",\"val\":[\"2\",\"3\"],\"type\":\"int\",\"field\":\"itemId\",\"conditions\":null},{\"id\":\"4\",\"pid\":\"2\",\"name\":null,\"op\":null,\"val\":null,\"type\":\"or\",\"field\":null,\"conditions\":[{\"id\":\"5\",\"pid\":\"4\",\"name\":\"threshold\",\"op\":\"gte\",\"val\":[\"95\"],\"type\":\"int\",\"field\":\"threshold\",\"conditions\":null},{\"id\":\"6\",\"pid\":\"4\",\"name\":\"isSeller\",\"op\":\"eq\",\"val\":[\"true\"],\"type\":\"boolean\",\"field\":\"isSeller\",\"conditions\":null}]}]}]}}";
        boolean result = ConditionParser.parser(conditionJson, param);
        System.out.println("evaluate result: " + result);
        System.out.println("evaluate path: ");
        NODE_CHAIN_FACTORY.printPath();
    }
}
