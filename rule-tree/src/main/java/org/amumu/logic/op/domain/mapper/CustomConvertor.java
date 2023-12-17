package org.amumu.logic.op.domain.mapper;

import org.amumu.logic.op.client.model.res.RuleTreeResultVO;
import org.amumu.logic.op.infra.functions.ruletree.path.Path;

public class CustomConvertor {

    public static RuleTreeResultVO.RuleTreePathVO root2root(Path path) {
        if (path == null) {
            return null;
        }
        RuleTreeResultVO.RuleTreePathVO root = new RuleTreeResultVO.RuleTreePathVO();
        root.setNext(root2root(path.getNext()));
        root.setResult(path.getResult());
        root.setName(path.getName());
        root.setId(path.getId());
        return root;
    }
}
