package org.amumu.rule.tree.domain.repo;

import org.amumu.rule.tree.infra.dao.model.RuleTreeDO;

import java.util.List;

public interface RuleTreeRepoService {
    List<RuleTreeDO> selectAll();

    RuleTreeDO selectById(Long id);

    int insert(RuleTreeDO ruleTreeDO);
}
