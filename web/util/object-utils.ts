export const updateObjectField =
  <ObjectType, Key extends keyof ObjectType>(
    key: Key,
    update: (value: ObjectType[Key]) => ObjectType[Key],
  ) =>
  (object: ObjectType) => ({ ...object, [key]: update(object[key]) });
