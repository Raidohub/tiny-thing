package org.amumu.rule.tree.infra.utils;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import org.amumu.rule.tree.client.exception.JsonUtilException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

/**
 * Utility class for JSON serialization and deserialization.
 */
@Slf4j
@Component
public class JsonUtil {
    @Setter
    @Autowired
    private static ObjectMapper OBJECT_MAPPER;

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
     * @param valType the target class for the object
     * @param <T> the type of the desired object
     * @return an object of type T
     */
    public static <T> T jsonNode2obj(JsonNode jsonNode, Class<T> valType) {
        try {
            return OBJECT_MAPPER.treeToValue(jsonNode, valType);
        } catch (JsonProcessingException e) {
            log.error("Error parsing JsonNode to {} class: {}", valType.getSimpleName(), e.getMessage());
            throw new JsonUtilException("Error parsing JsonNode to object", e);
        }
    }

    /**
     * Converts a JsonNode to a List of objects of the specified type.
     *
     * @param jsonNode the JsonNode to convert
     * @param valType the target class for the objects in the list
     * @param <T> the type of the desired objects
     * @return a List of objects of type T
     */
    public static <T> List<T> jsonNode2List(JsonNode jsonNode, Class<T> valType) {
        try {
            return OBJECT_MAPPER.readValue(OBJECT_MAPPER.treeAsTokens(jsonNode),
                    OBJECT_MAPPER.getTypeFactory().constructCollectionType(List.class, valType));
        } catch (Exception e) {
            log.error("Error parsing JsonNode to {} class: {}", valType.getSimpleName(), e.getMessage());
            throw new JsonUtilException("Error parsing JsonNode to list", e);
        }
    }

    /**
     * Converts a JsonNode containing an array of objects into a Map with designated keys.
     *
     * @param <K> The type of the keys in the resulting Map.
     * @param <V> The type of the values in the resulting Map.
     * @param jsonNode The JsonNode containing the JSON array to be converted.
     * @param valType The class of the value objects within the array.
     * @param retrieveKey A function that retrieves the key from a value object.
     * @return A Map with keys and values generated from the jsonNode array.
     * @throws JsonUtilException if parsing the JsonNode fails.
     */
    public static <K, V> Map<K, V> jsonNode2Map(JsonNode jsonNode, Class<V> valType, Function<V, K> retrieveKey) {
        if (jsonNode == null || !jsonNode.isArray()) {
            throw new IllegalArgumentException("The provided JsonNode must be an array node.");
        }

        try {
            List<V> valList = OBJECT_MAPPER.readValue(OBJECT_MAPPER.treeAsTokens(jsonNode),
                    OBJECT_MAPPER.getTypeFactory().constructCollectionType(List.class, valType));
            Map<K, V> result = new LinkedHashMap<>(valList.size());
            for (V v : valList) {
                K key = retrieveKey.apply(v);
                if (key == null) {
                    throw new IllegalStateException("The retrieveKey function must not return null.");
                }
                result.put(key, v);
            }
            return result;
        } catch (Exception e) {
            log.error("Error parsing JsonNode to {} class.", valType.getSimpleName(), e);
            throw new JsonUtilException("Error parsing JsonNode to map of type " + valType.getSimpleName(), e);
        }
    }

    /**
     * Converts a JSON string to an object of the specified type.
     *
     * @param jsonStr the JSON string to convert
     * @param valType the target class for the object
     * @param <T> the type of the desired object
     * @return an object of type T
     */
    public static <T> T str2obj(String jsonStr, Class<T> valType) {
        try {
            return OBJECT_MAPPER.readValue(jsonStr, valType);
        } catch (JsonProcessingException e) {
            log.error("Error parsing JSON string to {} class: {}", valType.getSimpleName(), e.getMessage());
            throw new JsonUtilException("Error parsing JSON string", e);
        }
    }

    /**
     * Converts a JSON string to a List of objects of the specified type.
     *
     * @param jsonStr the JSON string to convert
     * @param valType the target class for the objects in the list
     * @param <T> the type of the desired objects
     * @return a List of objects of type T
     */
    public static <T> List<T> str2List(String jsonStr, Class<T> valType) {
        try {
            return OBJECT_MAPPER.readValue(jsonStr,
                    OBJECT_MAPPER.getTypeFactory().constructCollectionType(List.class, valType));
        } catch (JsonProcessingException e) {
            log.error("Error parsing str to {} class: {}", valType.getSimpleName(), e.getMessage());
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
