package org.amumu.rule.tree.client.model.res;

import lombok.Data;

import java.util.List;

@Data
public class RuleTreeResultVO {
    private String param;
    private Boolean result;
    private RuleTreePathVO root;

    @Data
    public static class RuleTreePathVO {
        private String id;
        private String op;
        private String pid;
        private String type;
        private String name;
        private String param;
        private Boolean result;
        private List<String> val;
        private RuleTreePathVO son;
        private List<RuleTreePathVO> brothers;
    }
}
