package org.amumu.rule.tree.infra.repo;

import org.amumu.rule.tree.domain.repo.RuleTreeRepoService;
import org.amumu.rule.tree.infra.dao.model.RuleTreeDO;
import org.springframework.stereotype.Component;

import java.util.List;

@Component("ruleTreeStatics")
public class RuleTreeStaticsServiceImpl implements RuleTreeRepoService {
    @Override
    public List<RuleTreeDO> selectAll() {
        return null;
    }

    @Override
    public RuleTreeDO selectById(Long id) {
        return null;
    }

    @Override
    public int insert(RuleTreeDO ruleTreeDO) {
        return 0;
    }
}
