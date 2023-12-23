package org.amumu.rule.tree.domain.mapper;

import org.amumu.rule.tree.client.model.res.RuleTreeResultVO;
import org.amumu.rule.tree.infra.functions.ruletree.node.Node;

import java.util.stream.Collectors;

public class CustomConvertor {

    public static RuleTreeResultVO.RuleTreePathVO root2root(Node node) {
        if (node == null) {
            return null;
        }
        RuleTreeResultVO.RuleTreePathVO root = new RuleTreeResultVO.RuleTreePathVO();
        root.setBrothers(node.getBrothers().stream()
                .map(CustomConvertor::root2root).collect(Collectors.toList()));
        root.setSon(root2root(node.getSon()));
        root.setResult(node.getResult());
        root.setParam(node.getParam());
        root.setName(node.getName());
        root.setId(node.getId());
        return root;
    }
}
