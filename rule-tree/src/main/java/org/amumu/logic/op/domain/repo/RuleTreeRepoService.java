package org.amumu.logic.op.domain.repo;

import org.amumu.logic.op.infra.dao.model.RuleTreeDO;

import java.util.List;

public interface RuleTreeRepoService {
    List<RuleTreeDO> selectAll();

    RuleTreeDO selectById(Long id);

    int insert(RuleTreeDO ruleTreeDO);
}
