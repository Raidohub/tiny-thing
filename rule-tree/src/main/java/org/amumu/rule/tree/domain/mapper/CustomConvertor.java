package org.amumu.rule.tree.domain.mapper;

import org.amumu.rule.tree.client.model.res.RuleTreeResultVO;
import org.amumu.rule.tree.infra.functions.ruletree.path.Path;

import java.util.stream.Collectors;

public class CustomConvertor {

    public static RuleTreeResultVO.RuleTreePathVO root2root(Path path) {
        if (path == null) {
            return null;
        }
        RuleTreeResultVO.RuleTreePathVO root = new RuleTreeResultVO.RuleTreePathVO();
        root.setBrothers(path.getBrothers().stream()
                .map(CustomConvertor::root2root).collect(Collectors.toList()));
        root.setNext(root2root(path.getNext()));
        root.setResult(path.getResult());
        root.setParam(path.getParam());
        root.setName(path.getName());
        root.setId(path.getId());
        return root;
    }
}
