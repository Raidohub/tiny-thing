package org.amumu.logic.op.client.model.res;

import lombok.Data;

@Data
public class RuleTreeResultVO {
    private Boolean result;
    private RuleTreePathVO root;
    private String param;

    @Data
    public static class RuleTreePathVO {
        private String id;
        private String name;
        private String param;
        private Boolean result;
        private RuleTreePathVO next;
    }
}
