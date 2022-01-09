package com.bcdm.foodtraceability.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import java.time.LocalDate;
import com.baomidou.mybatisplus.annotation.TableId;
import java.time.LocalDateTime;
import java.io.Serializable;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * <p>
 * 用户信息
 * </p>
 *
 * @author 王
 * @since 2022-01-09
 */
@Data
  @EqualsAndHashCode(callSuper = false)
    public class UserInfo implements Serializable {

    private static final long serialVersionUID=1L;

      /**
     * 用户ID
     */
        @TableId(value = "user_id", type = IdType.AUTO)
      private Integer userId;

      /**
     * 用户状态
     */
      private Integer userStatus;

      /**
     * 姓名
     */
      private String userName;

      /**
     * 身份证号
     */
      private String idCard;

      /**
     * 电话号
     */
      private Integer phoneNumber;

      /**
     * 邮箱地址
     */
      private String mailAddress;

      /**
     * 性别
     */
      private Integer sex;

      /**
     * 年龄
     */
      private Integer age;

      /**
     * 出生日期
     */
      private LocalDate birthday;

    private LocalDateTime createTime;

    private LocalDateTime updateTime;


}
