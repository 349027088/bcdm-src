package com.bcdm.foodtraceability.entity;

import java.time.LocalDateTime;
import java.io.Serializable;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * <p>
 * 用户
 * </p>
 *
 * @author 王
 * @since 2022-01-09
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
     * UUID
     */
      private String salt;

    private LocalDateTime createTime;

    private LocalDateTime updateTime;


}
