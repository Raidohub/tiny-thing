package org.amumu.logic.op.domain.service;

import lombok.extern.slf4j.Slf4j;
import org.amumu.logic.op.client.model.req.RuleTreeParamReq;
import org.amumu.logic.op.client.model.req.RuleTreeRequest;
import org.amumu.logic.op.client.service.RuleTreeService;
import org.amumu.logic.op.domain.RuleTreeDomain;
import org.amumu.logic.op.domain.mapper.BeanMapper;
import org.amumu.logic.op.domain.repo.RuleTreeRepoService;
import org.amumu.logic.op.infra.dao.model.RuleTreeDO;
import org.amumu.logic.op.infra.utils.ConditionParser;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.List;

@Slf4j
@Service
public class RuleTreeServiceImpl implements RuleTreeService {

    @Autowired
    private RuleTreeRepoService ruleTreeRepoService;

    @Override
    public Integer createCondition(RuleTreeRequest ruleTreeRequest) {
        RuleTreeDomain ruleTreeDomain = BeanMapper.RULE_TREE_INSTANCE.req2Domain(ruleTreeRequest);
        ruleTreeDomain.init();

        RuleTreeDO ruleTreeDO = BeanMapper.RULE_TREE_INSTANCE.domain2DO(ruleTreeDomain);
        return ruleTreeRepoService.insert(ruleTreeDO);
    }

    @Override
    public Integer updateCondition(RuleTreeRequest ruleTreeRequest) {
        RuleTreeDO ruleTreeDO = ruleTreeRepoService.selectById(ruleTreeRequest.getId());
        if (ruleTreeDO == null) {
            log.info("rule tree not exist");
            return null;
        }

        RuleTreeDomain ruleTreeDomain = BeanMapper.RULE_TREE_INSTANCE.do2Domain(ruleTreeDO);
        ruleTreeDomain.upgrade(ruleTreeRequest);
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
    public Boolean parser(RuleTreeParamReq paramReq) {
        if (paramReq == null) {
            log.info("request is null");
            return null;
        }
        RuleTreeDO ruleTreeDO = ruleTreeRepoService.selectById(1L);
        if (ruleTreeDO == null) {
            log.info("ruleTreeDO is empty");
            return null;
        }
        return ConditionParser.parser(ruleTreeDO.getCondition(), paramReq);
    }
}
