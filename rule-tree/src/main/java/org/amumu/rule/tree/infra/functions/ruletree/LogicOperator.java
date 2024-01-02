package org.amumu.rule.tree.infra.functions.ruletree;

import com.fasterxml.jackson.databind.JsonNode;
import lombok.Setter;
import org.amumu.rule.tree.domain.RuleTreeConditionDomain;
import org.amumu.rule.tree.domain.model.RuleTreeParam;
import org.amumu.rule.tree.infra.utils.JsonUtil;
import org.apache.logging.log4j.util.Strings;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Collection;
import java.util.List;
import java.util.Map;

/**
 * 逻辑运算符表达式
 */
@Component
public class LogicOperator {

    @Setter
    @Autowired
    private static NodeChainFactory NODE_CHAIN_FACTORY;

    static class AND {
        public static boolean operate(JsonNode conditions, RuleTreeParam param) {
            boolean result = true;
            List<RuleTreeConditionDomain> subConditions = JsonUtil.jsonNode2List(conditions, RuleTreeConditionDomain.class);
            return doEvaluate(subConditions, param, result);
        }
    }

    static class OR {
        public static boolean operate(JsonNode conditions, RuleTreeParam param) {
            boolean result = false;
            Map<String, RuleTreeConditionDomain> subConditionMap = JsonUtil.jsonNode2Map(conditions, RuleTreeConditionDomain.class, RuleTreeConditionDomain::getId);
            // 根据id拿到指定的条件，如果指定的条件不存在，或者返回false，再遍历剩余条件
            if (Strings.isNotBlank(param.getId())) {
                RuleTreeConditionDomain subCondition = subConditionMap.get(param.getId());
                if (ConditionParser.evaluate(subCondition, param)) {
                    result = true;
                }
            }
            return doEvaluate(subConditionMap.values(), param, result);
        }
    }

    static class NOT {
        public static boolean operate(JsonNode conditions, RuleTreeParam param) {
            RuleTreeConditionDomain subCondition = JsonUtil.jsonNode2obj(conditions, RuleTreeConditionDomain.class);
            return !ConditionParser.evaluate(subCondition, param);
        }
    }

    private static boolean doEvaluate(Collection<RuleTreeConditionDomain> conditions, RuleTreeParam param, boolean initResult) {
        int idx = 0;
        for (RuleTreeConditionDomain subCondition : conditions) {
            if (idx > 0) {
                NODE_CHAIN_FACTORY.resetCurr();
            }
            if (ConditionParser.evaluate(subCondition, param) == !initResult) {
                initResult = !initResult;
            }
            idx++;
        }
        NODE_CHAIN_FACTORY.backtracePop();
        return initResult;
    }
}
