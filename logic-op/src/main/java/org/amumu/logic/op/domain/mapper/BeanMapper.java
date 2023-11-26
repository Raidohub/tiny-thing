package org.amumu.logic.op.domain.mapper;

import org.amumu.logic.op.client.model.req.ExpressionRequest;
import org.amumu.logic.op.client.model.res.ExpressionVO;
import org.amumu.logic.op.domain.ExpressionConditionDomain;
import org.amumu.logic.op.domain.ExpressionDomain;
import org.amumu.logic.op.infra.dao.model.ExpressionDO;
import org.amumu.logic.op.infra.utils.ConditionParser;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Named;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper
public interface BeanMapper {

    BeanMapper EXPRESSION_INSTANCE = Mappers.getMapper(BeanMapper.class);

    @Mapping(source = "condition", target = "condition", qualifiedByName = "condition2Str")
    ExpressionDO domain2DO(ExpressionDomain expressionDomain);
    @Mapping(source = "condition", target = "condition", qualifiedByName = "str2Condition")
    ExpressionDomain req2Domain(ExpressionRequest expressionRequest);

    @Mapping(source = "condition", target = "condition", qualifiedByName = "str2Condition")
    ExpressionDomain do2Domain(ExpressionDO expressionDO);
    List<ExpressionDomain> do2Domain(List<ExpressionDO> expressionDO);

    @Mapping(source = "condition", target = "condition", qualifiedByName = "condition2Str")
    ExpressionVO domain2VO(ExpressionDomain expressionDomain);
    List<ExpressionVO> domain2VO(List<ExpressionDomain> expressionDomain);

    @Named("str2Condition")
    default ExpressionConditionDomain str2Condition(String condition) {
        return ConditionParser.convert2Condition(condition);
    }

    @Named("condition2Str")
    default String condition2Str(ExpressionConditionDomain conditionStr) {
        return ConditionParser.convert2ConditionStr(conditionStr);
    }
}
