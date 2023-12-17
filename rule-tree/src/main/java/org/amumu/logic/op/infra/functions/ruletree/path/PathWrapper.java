package org.amumu.logic.op.infra.functions.ruletree.path;

import lombok.Data;

@Data
public class PathWrapper {
    private Path root;
    private Boolean result;
    private String ruleTreeParam;
}
