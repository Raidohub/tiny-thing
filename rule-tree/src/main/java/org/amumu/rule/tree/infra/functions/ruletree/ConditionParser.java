package org.amumu.rule.tree.infra.functions.ruletree;

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
            // 【获取子条件】
            return evaluate(subCondition, param);
        } else if (RuleTreeEnum.ENABLED.getName().equals(type)) {
            // 【开关】-返回开关的值【TRUE|FALSE]
            return enableOperate(condition);
        } else if (RuleTreeEnum.LogicalOperationEnum.enumValueOf(type) != null) {
            return logicOperate(condition, param, type);
        }
        return arithmeticOperate(condition, param);
    }

    /**
     * 逻辑运算
     * @param condition 表达式
     * @param param 参数
     * @param type AND OR NOT
     * @return boolean
     */
    private static boolean logicOperate(RuleTreeConditionDomain condition, RuleTreeParam param, String type) {
        NODE_CHAIN_FACTORY.inChain(null, condition, null);
        NODE_CHAIN_FACTORY.setupBacktracePoint();
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
        boolean result = ArithmeticOperator.enableOperate(condition);
        NODE_CHAIN_FACTORY.inChain(null, condition, result);
        return result;
    }

    /**
     * 执行表达式
     * @param condition 表达式
     * @param param 参数
     * @return boolean
     */
    private static boolean arithmeticOperate(RuleTreeConditionDomain condition, RuleTreeParam param) {
        String filed = condition.getField();
        String filedVal = retrieveFieldVal(param, filed);
        if (filedVal == null) {
            log.error("【{}】condition match retrieve【{}】return null", condition, filed);
            NODE_CHAIN_FACTORY.inChain(null, condition, false);
            return false;
        }

        String op = condition.getOp();
        List<String> valList = condition.getVal();
        boolean result = ArithmeticOperator.commonOperate(op, valList, filedVal);
        NODE_CHAIN_FACTORY.inChain(filedVal, condition, result);
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
        LogicOperator.setNODE_CHAIN_FACTORY(NODE_CHAIN_FACTORY);
        Map<String,String> map = new HashMap<>();
        map.put("threshold", "94");
        map.put("itemId", "200");
        RuleTreeParam param = new RuleTreeParam();
        param.setExtra(map);

        String conditionJson = "{\n" +
                "    \"id\": \"-1\",\n" +
                "    \"name\": \"签证时效切流配置\",\n" +
                "    \"op\": null,\n" +
                "    \"val\": null,\n" +
                "    \"type\": \"condition\",\n" +
                "    \"conditions\":\n" +
                "    {\n" +
                "        \"id\": \"0\",\n" +
                "        \"name\": null,\n" +
                "        \"op\": null,\n" +
                "        \"val\": null,\n" +
                "        \"type\": \"and\",\n" +
                "        \"field\": null,\n" +
                "        \"conditions\":\n" +
                "        [\n" +
                "            {\n" +
                "                \"id\": \"1\",\n" +
                "                \"name\": null,\n" +
                "                \"op\": null,\n" +
                "                \"val\": null,\n" +
                "                \"type\": \"or\",\n" +
                "                \"field\": null,\n" +
                "                \"conditions\":\n" +
                "                [\n" +
                "                    {\n" +
                "                        \"id\": \"10\",\n" +
                "                        \"name\": \"Switch\",\n" +
                "                        \"op\": null,\n" +
                "                        \"val\":\n" +
                "                        [\n" +
                "                            \"false\"\n" +
                "                        ],\n" +
                "                        \"type\": \"enabled\",\n" +
                "                        \"field\": null,\n" +
                "                        \"conditions\": null\n" +
                "                    },\n" +
                "                    {\n" +
                "\t\t                \"id\": \"11\",\n" +
                "\t\t                \"name\": null,\n" +
                "\t\t                \"op\": null,\n" +
                "\t\t                \"val\": null,\n" +
                "\t\t                \"type\": \"or\",\n" +
                "\t\t                \"field\": null,\n" +
                "\t\t                \"conditions\":\n" +
                "\t\t                [\n" +
                "\t\t                    {\n" +
                "\t\t                        \"id\": \"110\",\n" +
                "\t\t                        \"name\": \"buyerId\",\n" +
                "\t\t                        \"op\": null,\n" +
                "\t\t                        \"val\":\n" +
                "\t\t                        [\n" +
                "\t\t                            \"20\"\n" +
                "\t\t                        ],\n" +
                "\t\t                        \"type\": \"enabled\",\n" +
                "\t\t                        \"field\": null,\n" +
                "\t\t                        \"conditions\": null\n" +
                "\t\t                    },\n" +
                "\t\t                    {\n" +
                "\t\t                        \"id\": \"111\",\n" +
                "\t\t                        \"name\": \"Switch\",\n" +
                "\t\t                        \"op\": null,\n" +
                "\t\t                        \"val\":\n" +
                "\t\t                        [\n" +
                "\t\t                            \"true\"\n" +
                "\t\t                        ],\n" +
                "\t\t                        \"type\": \"enabled\",\n" +
                "\t\t                        \"field\": null,\n" +
                "\t\t                        \"conditions\": null\n" +
                "\t\t                    }\n" +
                "\t\t                ]\n" +
                "\t\t            }\n" +
                "                ]\n" +
                "            },\n" +
                "            {\n" +
                "                \"id\": \"2\",\n" +
                "                \"name\": null,\n" +
                "                \"op\": null,\n" +
                "                \"val\": null,\n" +
                "                \"type\": \"or\",\n" +
                "                \"field\": null,\n" +
                "                \"conditions\":\n" +
                "                [\n" +
                "                    {\n" +
                "                        \"id\": \"3\",\n" +
                "                        \"name\": \"itemId\",\n" +
                "                        \"op\": \"in\",\n" +
                "                        \"val\":\n" +
                "                        [\n" +
                "                            \"2\",\n" +
                "                            \"3\"\n" +
                "                        ],\n" +
                "                        \"type\": \"int\",\n" +
                "                        \"field\": \"itemId\",\n" +
                "                        \"conditions\": null\n" +
                "                    },\n" +
                "                    {\n" +
                "                        \"id\": \"4\",\n" +
                "                        \"name\": null,\n" +
                "                        \"op\": null,\n" +
                "                        \"val\": null,\n" +
                "                        \"type\": \"or\",\n" +
                "                        \"field\": null,\n" +
                "                        \"conditions\":\n" +
                "                        [\n" +
                "                            {\n" +
                "                                \"id\": \"5\",\n" +
                "                                \"name\": \"threshold\",\n" +
                "                                \"op\": \"gte\",\n" +
                "                                \"val\":\n" +
                "                                [\n" +
                "                                    \"95\"\n" +
                "                                ],\n" +
                "                                \"type\": \"int\",\n" +
                "                                \"field\": \"threshold\",\n" +
                "                                \"conditions\": null\n" +
                "                            },\n" +
                "                            {\n" +
                "                                \"id\": \"6\",\n" +
                "                                \"name\": \"isSeller\",\n" +
                "                                \"op\": \"eq\",\n" +
                "                                \"val\":\n" +
                "                                [\n" +
                "                                    \"true\"\n" +
                "                                ],\n" +
                "                                \"type\": \"boolean\",\n" +
                "                                \"field\": \"isSeller\",\n" +
                "                                \"conditions\": null\n" +
                "                            }\n" +
                "                        ]\n" +
                "                    }\n" +
                "                ]\n" +
                "            }\n" +
                "        ]\n" +
                "    }\n" +
                "}";
        boolean result = ConditionParser.parser(conditionJson, param);
        System.out.println("evaluate result: " + result);
        System.out.println("evaluate path: ");
        NODE_CHAIN_FACTORY.printPath();
    }
}
