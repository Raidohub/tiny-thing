package org.amumu.rule.tree.client.service;

import org.amumu.rule.tree.client.model.req.RuleTreeParamReq;
import org.amumu.rule.tree.client.model.req.RuleTreeReq;
import org.amumu.rule.tree.client.model.res.RuleTreeResultVO;
import org.amumu.rule.tree.domain.RuleTreeDomain;

import java.util.List;

public interface RuleTreeService {
    Integer createCondition(RuleTreeReq ruleTreeReq);

    Integer updateCondition(RuleTreeReq ruleTreeReq);

    List<RuleTreeDomain> selectList();

    RuleTreeResultVO parser(RuleTreeParamReq paramReq);
}
