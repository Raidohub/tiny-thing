package org.amumu.logic.op.infra.config;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.amumu.logic.op.infra.functions.ruletree.ConditionParser;
import org.amumu.logic.op.infra.functions.ruletree.PathChainFactory;
import org.amumu.logic.op.infra.utils.JsonUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class StaticUtilWrapper {

    private PathChainFactory pathChainFactory;
    private ObjectMapper objectMapper;

    @Autowired
    public StaticUtilWrapper(PathChainFactory pathChainFactory, ObjectMapper objectMapper) {
        this.pathChainFactory = pathChainFactory;
        this.objectMapper = objectMapper;
        this.initStaticUtil();
    }

    private void initStaticUtil() {
        ConditionParser.setPathChainFactory(pathChainFactory);
        JsonUtil.setPathChainFactory(objectMapper);
    }
}
