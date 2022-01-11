package com.bcdm.foodtraceability.entity;

import java.time.LocalDateTime;
import java.io.Serializable;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * <p>
 * 
 * </p>
 *
 * @author 王
 * @since 2022-01-11
 */
@Data
@EqualsAndHashCode(callSuper = false)
public class User implements Serializable {

    private static final long serialVersionUID=1L;

    /**
     * 账号
     */
    private String loginId;

    /**
     * 密码
     */
    private String password;

    /**
     * 用户ID
     */
    private Integer userId;

    /**
     * 用户状态
     */
    private Integer userStatus;

    /**
     * UUID
     */
    private String salt;

    private LocalDateTime createTime;

    private LocalDateTime updateTime;


}
