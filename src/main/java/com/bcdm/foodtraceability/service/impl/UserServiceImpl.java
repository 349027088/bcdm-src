package com.bcdm.foodtraceability.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.bcdm.foodtraceability.entity.User;
import com.bcdm.foodtraceability.exception.ServiceBusinessException;
import com.bcdm.foodtraceability.mapper.UserMapper;
import com.bcdm.foodtraceability.service.UserService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;

import static com.bcdm.foodtraceability.common.Constants.USER_STATUS_LOCK;
import static com.bcdm.foodtraceability.common.Constants.USER_STATUS_UNLOCK;
import static com.bcdm.foodtraceability.common.CreateMD5.Md5encode;
import static com.bcdm.foodtraceability.common.CreateUUID.getUUID;
import static com.bcdm.foodtraceability.common.HttpConstants.HTTP_RETURN_FAIL;

/**
 * <p>
 * 服务实现类
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
@Service
@Slf4j
public class UserServiceImpl extends ServiceImpl<UserMapper, User> implements UserService {

    @Override
    public User login(User user) throws Exception {
        log.info(user.getLoginId() + "-------登录");
        QueryWrapper<User> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("login_id", user.getLoginId());
        User selectUser = getOne(queryWrapper);
        if (!(null == selectUser)) {
            StringBuilder stringBuilder = new StringBuilder();
            stringBuilder.append(user.getPassword());
            stringBuilder.append(selectUser.getSalt());
            if (Md5encode(stringBuilder.toString()).equals(selectUser.getPassword())) {
                return selectUser;
            }
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, "账号密码错误");
    }

    @Override
    public User register(User user) throws Exception {
        log.info(user.getLoginId() + "-------注册");
        QueryWrapper<User> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("login_id", user.getLoginId());
        if (0 == count(queryWrapper)) {
            user.setSalt(getUUID());
            String password = user.getPassword();
            String stringBuffer = user.getPassword() + user.getSalt();
            user.setPassword(Md5encode(stringBuffer));
            LocalDateTime now = LocalDateTime.now();
            user.setUserStatus(USER_STATUS_UNLOCK);
            user.setCreateTime(now);
            user.setUpdateTime(now);
            save(user);
            user.setPassword(password);
            return login(user);
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, "当前账号已被使用");
    }

    @Override
    public User modifyPassword(User user, String newPassword) throws Exception {
        log.info(user.getLoginId() + "-------修改密码");
        User targetUser = login(user);
        String stringBuffer = newPassword + targetUser.getSalt();
        targetUser.setPassword(Md5encode(stringBuffer));
        UpdateWrapper<User> updateWrapper = new UpdateWrapper<>();
        updateWrapper.eq("user_id", targetUser.getUserId());
        updateWrapper.eq("update_time", targetUser.getUpdateTime());
        LocalDateTime now = LocalDateTime.now();
        user.setUpdateTime(now);
        if (update(targetUser, updateWrapper)) {
            return targetUser;
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, "密码修改失败");
    }

    @Override
    public User modifyUserInfo(User user) throws Exception {
        log.info(user.getLoginId() + "-------修改用户信息");
        User targetUser = login(user);
        UpdateWrapper<User> updateWrapper = new UpdateWrapper<>();
        updateWrapper.eq("user_id", targetUser.getUserId());
        updateWrapper.eq("update_time", targetUser.getUpdateTime());
        LocalDateTime now = LocalDateTime.now();
        user.setUpdateTime(now);
        return update(user, updateWrapper) ? user : targetUser;
    }

    @Override
    public boolean lockUser(User user) throws Exception {
        log.info(user.getLoginId() + "-------锁定用户");
        user.setUserStatus(USER_STATUS_LOCK);
        UpdateWrapper<User> updateWrapper = forStatusUpdate(user);
        LocalDateTime now = LocalDateTime.now();
        user.setUpdateTime(now);
        return update(user, updateWrapper);
    }

    @Override
    public boolean unLockUser(User user) throws Exception {
        log.info(user.getLoginId() + "-------解锁用户");
        user.setUserStatus(USER_STATUS_UNLOCK);
        UpdateWrapper<User> updateWrapper = forStatusUpdate(user);
        user.setUpdateTime(LocalDateTime.now());
        return update(user, updateWrapper);
    }

    /**
     * 用户锁定解锁用共通处理
     * @param user 用户
     * @return 生成的SQL操作
     */
    private UpdateWrapper<User> forStatusUpdate(User user){
        UpdateWrapper<User> updateWrapper = new UpdateWrapper<>();
        updateWrapper.eq("user_id", user.getUserId());
        updateWrapper.set("user_status", user.getUserStatus());
        updateWrapper.eq("update_time", user.getUpdateTime());
        return updateWrapper;
    }
}
