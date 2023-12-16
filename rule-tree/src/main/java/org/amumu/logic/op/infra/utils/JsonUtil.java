package org.amumu.logic.op.infra.utils;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.amumu.logic.op.client.exception.JsonUtilException;

import java.util.List;

/**
 * Utility class for JSON serialization and deserialization.
 */
@Slf4j
public class JsonUtil {
    private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();

    private JsonUtil() {
        // Private constructor to prevent instantiation
    }

    /**
     * Converts a JSON string to a JsonNode object.
     *
     * @param jsonStr the JSON string to convert
     * @return the JsonNode representation of the JSON string
     */
    public static JsonNode str2jsonNode(String jsonStr) {
        try {
            return OBJECT_MAPPER.readTree(jsonStr);
        } catch (JsonProcessingException e) {
            log.error("Error parsing JSON string to JsonNode: {}", e.getMessage());
            throw new JsonUtilException("Error parsing JSON string to JsonNode", e);
        }
    }

    /**
     * Converts a JsonNode to an object of the specified type.
     *
     * @param jsonNode the JsonNode to convert
     * @param valueType the target class for the object
     * @param <T> the type of the desired object
     * @return an object of type T
     */
    public static <T> T jsonNode2obj(JsonNode jsonNode, Class<T> valueType) {
        try {
            return OBJECT_MAPPER.treeToValue(jsonNode, valueType);
        } catch (JsonProcessingException e) {
            log.error("Error parsing JsonNode to {} class: {}", valueType.getSimpleName(), e.getMessage());
            throw new JsonUtilException("Error parsing JsonNode to object", e);
        }
    }

    /**
     * Converts a JsonNode to a List of objects of the specified type.
     *
     * @param jsonNode the JsonNode to convert
     * @param valueType the target class for the objects in the list
     * @param <T> the type of the desired objects
     * @return a List of objects of type T
     */
    public static <T> List<T> jsonNode2List(JsonNode jsonNode, Class<T> valueType) {
        try {
            return OBJECT_MAPPER.readValue(OBJECT_MAPPER.treeAsTokens(jsonNode),
                    OBJECT_MAPPER.getTypeFactory().constructCollectionType(List.class, valueType));
        } catch (Exception e) {
            log.error("Error parsing JsonNode to {} class: {}", valueType.getSimpleName(), e.getMessage());
            throw new JsonUtilException("Error parsing JsonNode to list", e);
        }
    }

    /**
     * Converts a JSON string to an object of the specified type.
     *
     * @param jsonStr the JSON string to convert
     * @param valueType the target class for the object
     * @param <T> the type of the desired object
     * @return an object of type T
     */
    public static <T> T str2obj(String jsonStr, Class<T> valueType) {
        try {
            return OBJECT_MAPPER.readValue(jsonStr, valueType);
        } catch (JsonProcessingException e) {
            log.error("Error parsing JSON string to {} class: {}", valueType.getSimpleName(), e.getMessage());
            throw new JsonUtilException("Error parsing JSON string", e);
        }
    }

    /**
     * Converts a JSON string to a List of objects of the specified type.
     *
     * @param jsonStr the JSON string to convert
     * @param valueType the target class for the objects in the list
     * @param <T> the type of the desired objects
     * @return a List of objects of type T
     */
    public static <T> List<T> str2List(String jsonStr, Class<T> valueType) {
        try {
            return OBJECT_MAPPER.readValue(jsonStr,
                    OBJECT_MAPPER.getTypeFactory().constructCollectionType(List.class, valueType));
        } catch (JsonProcessingException e) {
            log.error("Error parsing str to {} class: {}", valueType.getSimpleName(), e.getMessage());
            throw new JsonUtilException("Error parsing JSON array", e);
        }
    }

    /**
     * Converts an object to a JSON string.
     *
     * @param obj the object to convert
     * @return a JSON string representation of the object
     */
    public static String obj2JsonStr(Object obj) {
        try {
            return OBJECT_MAPPER.writeValueAsString(obj);
        } catch (JsonProcessingException e) {
            log.error("Error converting object to JSON string: {}", e.getMessage());
            throw new JsonUtilException("Error converting object to JSON string", e);
        }
    }
}
