package org.amumu.rule.tree.infra.functions.ruletree.node;

import lombok.Data;

@Data
public class NodeWrapper {
    private Node root;
    private Boolean result;
    private String param;
}
