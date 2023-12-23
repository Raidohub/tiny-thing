package org.amumu.rule.tree.infra.functions.ruletree;

import com.fasterxml.jackson.databind.JsonNode;
import org.amumu.rule.tree.domain.RuleTreeConditionDomain;
import org.amumu.rule.tree.domain.model.RuleTreeParam;
import org.amumu.rule.tree.infra.utils.JsonUtil;
import org.apache.logging.log4j.util.Strings;
import java.util.List;
import java.util.Map;

/**
 * 逻辑运算符表达式
 */
public class LogicOperator {

    static class AND {
        public static boolean operate(JsonNode conditions, RuleTreeParam param) {
            List<RuleTreeConditionDomain> subConditions = JsonUtil.jsonNode2List(conditions, RuleTreeConditionDomain.class);
            for (RuleTreeConditionDomain subCondition : subConditions) {
                if (!ConditionParser.evaluate(subCondition, param)) {
                    return false;
                }
            }
            return true;
        }
    }

    static class OR {
        public static boolean operate(JsonNode conditions, RuleTreeParam param) {
            Map<String, RuleTreeConditionDomain> subConditionMap = JsonUtil.jsonNode2Map(conditions, RuleTreeConditionDomain.class, RuleTreeConditionDomain::getId);
            // 根据id拿到指定的条件，如果指定的条件不存在，或者返回false，再遍历剩余条件
            if (Strings.isNotBlank(param.getId())) {
                RuleTreeConditionDomain subCondition = subConditionMap.get(param.getId());
                if (ConditionParser.evaluate(subCondition, param)) {
                    return true;
                }
            }
            for (RuleTreeConditionDomain subCondition : subConditionMap.values()) {
                if (ConditionParser.evaluate(subCondition, param)) {
                    return true;
                }
            }
            return false;
        }
    }

    static class NOT {
        public static boolean operate(JsonNode conditions, RuleTreeParam param) {
            RuleTreeConditionDomain subCondition = JsonUtil.jsonNode2obj(conditions, RuleTreeConditionDomain.class);
            return !ConditionParser.evaluate(subCondition, param);
        }
    }
}
