package org.amumu.rule.tree.domain.service;

import lombok.extern.slf4j.Slf4j;
import org.amumu.rule.tree.client.model.req.RuleTreeParamReq;
import org.amumu.rule.tree.client.model.req.RuleTreeReq;
import org.amumu.rule.tree.client.model.res.RuleTreeResultVO;
import org.amumu.rule.tree.client.service.RuleTreeService;
import org.amumu.rule.tree.domain.RuleTreeDomain;
import org.amumu.rule.tree.domain.mapper.BeanMapper;
import org.amumu.rule.tree.domain.repo.RuleTreeRepoService;
import org.amumu.rule.tree.infra.dao.model.RuleTreeDO;
import org.amumu.rule.tree.infra.functions.ruletree.ConditionParser;
import org.amumu.rule.tree.infra.functions.ruletree.NodeChainFactory;
import org.amumu.rule.tree.infra.functions.ruletree.node.NodeWrapper;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.List;
import java.util.Optional;

@Slf4j
@Service
public class RuleTreeServiceImpl implements RuleTreeService {

    @Autowired
    private NodeChainFactory nodeChainFactory;

    @Autowired
    private RuleTreeRepoService ruleTreeRepoService;

    @Override
    public Integer createCondition(RuleTreeReq ruleTreeReq) {
        RuleTreeDomain ruleTreeDomain = BeanMapper.RULE_TREE_INSTANCE.req2Domain(ruleTreeReq);
        ruleTreeDomain.init();

        RuleTreeDO ruleTreeDO = BeanMapper.RULE_TREE_INSTANCE.domain2DO(ruleTreeDomain);
        return ruleTreeRepoService.insert(ruleTreeDO);
    }

    @Override
    public Integer updateCondition(RuleTreeReq ruleTreeReq) {
        RuleTreeDO ruleTreeDO = ruleTreeRepoService.selectById(ruleTreeReq.getId());
        if (ruleTreeDO == null) {
            log.info("rule tree not exist");
            return null;
        }

        RuleTreeDomain ruleTreeDomain = BeanMapper.RULE_TREE_INSTANCE.do2Domain(ruleTreeDO);
        ruleTreeDomain.upgrade(ruleTreeReq);
        RuleTreeDO ruleTreeDOUpgrade = BeanMapper.RULE_TREE_INSTANCE.domain2DO(ruleTreeDomain);
        return ruleTreeRepoService.insert(ruleTreeDOUpgrade);
    }

    @Override
    public List<RuleTreeDomain> selectList() {
        List<RuleTreeDO> ruleTreeDOList = ruleTreeRepoService.selectAll();
        if (ruleTreeDOList.isEmpty()) {
            log.info("ruleTreeDOList is empty");
            return Collections.emptyList();
        }

        return BeanMapper.RULE_TREE_INSTANCE.do2Domain(ruleTreeDOList);
    }

    @Override
    public RuleTreeResultVO parser(RuleTreeParamReq paramReq) {
        if (paramReq == null) {
            log.info("request is null");
            return null;
        }

        Boolean result = Optional.ofNullable(paramReq.getCondition())
                .map(condition -> this.doParser(paramReq))
                .orElseGet(() -> {
                    RuleTreeDO ruleTreeDO = ruleTreeRepoService.selectById(1L);
                    if (ruleTreeDO == null) {
                        log.info("ruleTreeDO is empty");
                        return null;
                    }
                    paramReq.setCondition(ruleTreeDO.getCondition());
                    return this.doParser(paramReq);
                });
        NodeWrapper nodeWrapper = nodeChainFactory.buildNode(result, paramReq);
        return BeanMapper.RULE_TREE_INSTANCE.convertRuleTreePath(nodeWrapper);
    }

    private boolean doParser(RuleTreeParamReq paramReq) {
        if (null == paramReq) {
            log.info("request param not exist");
            return false;
        }
        if (StringUtils.isBlank(paramReq.getCondition())) {
            log.info("condition not exist");
            return false;
        }

        try {
            return ConditionParser.parser(paramReq.getCondition(), paramReq);
        } catch (Exception e) {
            log.info("parser error");
        }
        return false;
    }
}
