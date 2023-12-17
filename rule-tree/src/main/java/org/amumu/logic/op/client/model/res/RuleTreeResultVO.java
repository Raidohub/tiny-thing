package org.amumu.logic.op.client.model.res;

import lombok.Data;

@Data
public class RuleTreeResultVO {
    private Boolean result;
    private RuleTreePathVO root;
    private String ruleTreeParam;

    @Data
    public static class RuleTreePathVO {
        private String id;
        private String name;
        private Boolean result;
        private RuleTreePathVO next;
    }
}
