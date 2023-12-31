package org.amumu.rule.tree.domain.mapper;

import org.amumu.rule.tree.client.model.req.RuleTreeReq;
import org.amumu.rule.tree.client.model.res.RuleTreeResultVO;
import org.amumu.rule.tree.client.model.res.RuleTreeVO;
import org.amumu.rule.tree.domain.RuleTreeConditionDomain;
import org.amumu.rule.tree.domain.RuleTreeDomain;
import org.amumu.rule.tree.infra.dao.model.RuleTreeDO;
import org.amumu.rule.tree.infra.functions.ruletree.node.Node;
import org.amumu.rule.tree.infra.functions.ruletree.node.NodeWrapper;
import org.amumu.rule.tree.infra.utils.JsonUtil;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Named;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper
public interface BeanMapper {

    BeanMapper RULE_TREE_INSTANCE = Mappers.getMapper(BeanMapper.class);

    @Mapping(source = "condition", target = "condition", qualifiedByName = "condition2Str")
    RuleTreeDO domain2DO(RuleTreeDomain ruleTreeDomain);
    @Mapping(source = "condition", target = "condition", qualifiedByName = "str2Condition")
    RuleTreeDomain req2Domain(RuleTreeReq ruleTreeReq);

    @Mapping(source = "condition", target = "condition", qualifiedByName = "str2Condition")
    RuleTreeDomain do2Domain(RuleTreeDO ruleTreeDO);
    List<RuleTreeDomain> do2Domain(List<RuleTreeDO> ruleTreeDO);

    @Mapping(source = "condition", target = "condition", qualifiedByName = "condition2Str")
    RuleTreeVO domain2VO(RuleTreeDomain ruleTreeDomain);
    List<RuleTreeVO> domain2VO(List<RuleTreeDomain> ruleTreeDomain);

    @Mapping(source = "root", target = "root", qualifiedByName = "root2root")
    RuleTreeResultVO convertRuleTreePath(NodeWrapper wrapper);

    @Named("str2Condition")
    default RuleTreeConditionDomain str2Condition(String condition) {
        return JsonUtil.str2obj(condition, RuleTreeConditionDomain.class);
    }

    @Named("condition2Str")
    default String condition2Str(RuleTreeConditionDomain conditionStr) {
        return JsonUtil.obj2JsonStr(conditionStr);
    }

    @Named("root2root")
    default RuleTreeResultVO.RuleTreePathVO root2root(Node node) {
        return CustomConvertor.root2root(node);
    }
}
