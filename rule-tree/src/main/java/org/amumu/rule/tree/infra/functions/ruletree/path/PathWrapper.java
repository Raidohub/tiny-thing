package org.amumu.rule.tree.infra.functions.ruletree.path;

import lombok.Data;

@Data
public class PathWrapper {
    private Path root;
    private Boolean result;
    private String param;
}
