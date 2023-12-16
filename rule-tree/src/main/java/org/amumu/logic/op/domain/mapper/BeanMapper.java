package org.amumu.logic.op.domain.mapper;

import org.amumu.logic.op.client.model.req.RuleTreeRequest;
import org.amumu.logic.op.client.model.res.RuleTreeVO;
import org.amumu.logic.op.domain.RuleTreeConditionDomain;
import org.amumu.logic.op.domain.RuleTreeDomain;
import org.amumu.logic.op.infra.dao.model.RuleTreeDO;
import org.amumu.logic.op.infra.functions.ruletree.ConditionParser;
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
    RuleTreeDomain req2Domain(RuleTreeRequest ruleTreeRequest);

    @Mapping(source = "condition", target = "condition", qualifiedByName = "str2Condition")
    RuleTreeDomain do2Domain(RuleTreeDO ruleTreeDO);
    List<RuleTreeDomain> do2Domain(List<RuleTreeDO> ruleTreeDO);

    @Mapping(source = "condition", target = "condition", qualifiedByName = "condition2Str")
    RuleTreeVO domain2VO(RuleTreeDomain ruleTreeDomain);
    List<RuleTreeVO> domain2VO(List<RuleTreeDomain> ruleTreeDomain);

    @Named("str2Condition")
    default RuleTreeConditionDomain str2Condition(String condition) {
        return ConditionParser.convert2Condition(condition);
    }

    @Named("condition2Str")
    default String condition2Str(RuleTreeConditionDomain conditionStr) {
        return ConditionParser.convert2ConditionStr(conditionStr);
    }
}
