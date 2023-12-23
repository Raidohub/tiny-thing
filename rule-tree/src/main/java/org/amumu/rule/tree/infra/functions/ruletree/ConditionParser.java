package org.amumu.rule.tree.infra.functions.ruletree;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.amumu.rule.tree.client.RuleTreeEnum;
import org.amumu.rule.tree.domain.RuleTreeConditionDomain;
import org.amumu.rule.tree.domain.model.RuleTreeParam;
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
        String id = Optional.ofNullable(condition).map(RuleTreeConditionDomain::getId).orElse(null);
        String name = Optional.ofNullable(condition).map(RuleTreeConditionDomain::getName).orElse(null);

        NODE_CHAIN_FACTORY.initPath(id, name);
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
            return enableExpress(condition);
        } else if (RuleTreeEnum.LogicalOperationEnum.AND.getName().equals(type)) {
            return LogicOperator.AND.operate(condition.getConditions(), param);
        } else if (RuleTreeEnum.LogicalOperationEnum.OR.getName().equals(type)) {
            return LogicOperator.OR.operate(condition.getConditions(), param);
        } else if (RuleTreeEnum.LogicalOperationEnum.NOT.getName().equals(type)) {
            return LogicOperator.NOT.operate(condition.getConditions(), param);
        }
        return operatorExpress(condition, param);
    }

    /**
     * 执行开关表达式
     * @param condition 表达式
     * @return boolean
     */
    private static boolean enableExpress(RuleTreeConditionDomain condition) {
        boolean result = Operator.enableOperate(condition);
        NODE_CHAIN_FACTORY.next(null, condition, result);
        return result;
    }

    /**
     * 执行表达式
     * @param condition 表达式
     * @param param 参数
     * @return boolean
     */
    private static boolean operatorExpress(RuleTreeConditionDomain condition, RuleTreeParam param) {
        String filed = condition.getField();
        String filedVal = retrieveFieldVal(param, filed);
        if (filedVal == null) {
            log.error("【{}】condition match retrieve【{}】return null", condition, filed);
            NODE_CHAIN_FACTORY.next(null, condition, false);
            return false;
        }

        String op = condition.getOp();
        List<String> valList = condition.getVal();
        boolean result = Operator.commonOperate(op, valList, filedVal);
        NODE_CHAIN_FACTORY.next(filedVal, condition, result);
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
        JsonUtil.setOBJECT_MAPPER(new ObjectMapper());
        Map<String,String> map = new HashMap<>();
        map.put("threshold", "94");
        map.put("itemId", "200");
        RuleTreeParam param = new RuleTreeParam();
        param.setExtra(map);

        String conditionJson = "{\"id\":\"-1\",\"name\":\"签证时效切流配置\",\"op\":null,\"val\":null,\"type\":\"condition\",\"conditions\":{\"id\":\"0\",\"name\":null,\"op\":null,\"val\":null,\"type\":\"and\",\"field\":null,\"conditions\":[{\"id\":\"1\",\"name\":\"xxxSwitch\",\"op\":null,\"val\":[\"true\"],\"type\":\"enabled\",\"field\":null,\"conditions\":null},{\"id\":\"2\",\"name\":null,\"op\":null,\"val\":null,\"type\":\"or\",\"field\":null,\"conditions\":[{\"id\":\"3\",\"name\":\"itemId\",\"op\":\"in\",\"val\":[\"2\",\"3\"],\"type\":\"int\",\"field\":\"itemId\",\"conditions\":null},{\"id\":\"4\",\"name\":null,\"op\":null,\"val\":null,\"type\":\"condition\",\"field\":null,\"conditions\":{\"id\":0,\"name\":null,\"op\":null,\"val\":null,\"type\":\"or\",\"field\":null,\"conditions\":[{\"name\":\"threshold\",\"id\":\"5\",\"op\":\"gte\",\"val\":[\"95\"],\"type\":\"int\",\"field\":\"threshold\",\"conditions\":null},{\"name\":\"isSeller\",\"id\":\"6\",\"op\":\"eq\",\"val\":[\"true\"],\"type\":\"boolean\",\"field\":\"isSeller\",\"conditions\":null}]}}]}]}}";
        boolean result = ConditionParser.parser(conditionJson, param);
        System.out.println("evaluate result: " + result);
        System.out.println("evaluate path: ");
        NODE_CHAIN_FACTORY.printPath();
    }
}
